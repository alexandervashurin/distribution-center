(ns distribution-center.simulation
  (:require [clojure.core.async :as async :refer [go go-loop chan <! >! timeout alts! close!]]
            [distribution-center.models :as m]
            [distribution-center.warehouse :as wh]
            [distribution-center.stats :as st]
            [distribution-center.ui :as ui])
  (:import [java.time LocalDateTime]))

;; Каналы
(def unloading-queue (chan 20))  ; Очередь разгрузки
(def loading-queue (chan 5))     ; Канал загрузки (5 портов)
(def control-chan (chan 1))      ; Канал управления

;; Генерация товаров для грузовика
(defn generate-items [max-weight]
  (loop [remaining max-weight
         items []]
    (if (<= remaining 1000)
      items
      (let [item-type (rand-nth m/item-types)
            category (rand-nth (:categories item-type))
            units (inc (rand-int 5))
            weight (* units (:weight category))
            time (* units (:time category))]
        (if (> weight remaining)
          items
          (recur (- remaining weight)
                 (conj items {:type (:type item-type)
                              :category (:id category)
                              :category-name (:name category)
                              :weight weight
                              :time time
                              :units units})))))))

;; Генерация грузовика на разгрузку
(defn generate-unloading-truck []
  (let [truck-type (rand-nth m/truck-types)
        items (generate-items (:capacity truck-type))]
    {:id (str "U-" (System/currentTimeMillis))
     :type (:id truck-type)
     :name (:name truck-type)
     :capacity (:capacity truck-type)
     :items items
     :total-weight (reduce + (map :weight items))}))

;; Генерация грузовика на загрузку (только малый и средний)
(defn generate-loading-truck []
  (let [truck-type (rand-nth (filter #(#{:small :medium} (:id %)) m/truck-types))
        item-type (rand-nth m/item-types)
        category (rand-nth (:categories item-type))]
    {:id (str "L-" (System/currentTimeMillis))
     :type (:id truck-type)
     :name (:name truck-type)
     :capacity (:capacity truck-type)
     :item-type (:type item-type)
     :category (:id category)
     :category-name (:name category)}))

;; Порт разгрузки
(defn simulate-unloading-port [port-id]
  (go-loop []
    (let [[truck ch] (alts! [unloading-queue control-chan])]
      (cond
        (= ch control-chan) 
        (ui/log :info (str "Порт разгрузки #" port-id " остановлен"))
        
        truck
        (do
          (ui/log :unloading (str "Порт разгрузки #" port-id ": начало разгрузки грузовика " 
                                 (:id truck) " (" (:name truck) ", товаров: " (count (:items truck)) ")"))
          
          (doseq [item (:items truck)]
            (ui/log :unloading (str "  Порт #" port-id ": разгрузка " (:units item) " ед. " 
                                   (:category-name item) " (" (:weight item) " кг)"))
            (<! (timeout (* 100 (:time item))))
            (wh/add-item (:type item) (:category item) (:weight item))
            (st/inc! :warehouse-operations)
            (ui/log :warehouse (str "  Склад: добавлено " (:weight item) " кг " (:category-name item) 
                                   " (всего: " (wh/get-stock (:type item) (:category item)) " кг)")))
          
          (st/inc! :unloaded-trucks)
          (st/inc! :total-unloaded-weight (:total-weight truck))
          (ui/log :departure (str "Порт разгрузки #" port-id ": грузовик " (:id truck) 
                                 " разгружен и покинул центр (вес: " (:total-weight truck) " кг)"))
          (recur))))))

;; Порт загрузки
(defn simulate-loading-port [port-id]
  (go-loop []
    (let [[truck ch] (alts! [loading-queue control-chan])]
      (cond
        (= ch control-chan)
        (ui/log :info (str "Порт загрузки #" port-id " остановлен"))
        
        truck
        (do
          (ui/log :loading (str "Порт загрузки #" port-id ": начало загрузки грузовика " 
                               (:id truck) " (" (:name truck) " для " (:category-name truck) ")"))
          
          (loop [remaining (:capacity truck)
                 loaded 0]
            (let [available (wh/get-stock (:item-type truck) (:category truck))]
              (if (pos? available)
                (let [load-weight (min remaining available)
                      item-type-info (first (filter #(= (:type %) (:item-type truck)) m/item-types))
                      category-info (first (filter #(= (:id %) (:category truck)) (:categories item-type-info)))
                      unit-weight (:weight category-info)
                      units (max 1 (int (/ load-weight (max 1 unit-weight))))]
                  (ui/log :loading (str "  Порт #" port-id ": загрузка " units " ед. " (:category-name truck) 
                                       " (" load-weight " кг)"))
                  (<! (timeout (* 100 (max 1 (int (/ load-weight 10))))))
                  (wh/remove-item (:item-type truck) (:category truck) load-weight)
                  (st/inc! :warehouse-operations)
                  (ui/log :warehouse (str "  Склад: извлечено " load-weight " кг " (:category-name truck) 
                                         " (осталось: " (wh/get-stock (:item-type truck) (:category truck)) " кг)"))
                  
                  (let [new-loaded (+ loaded load-weight)]
                    (if (>= load-weight remaining)
                      (do
                        (st/inc! :loaded-trucks)
                        (st/inc! :total-loaded-weight new-loaded)
                        (ui/log :departure (str "Порт загрузки #" port-id ": грузовик " (:id truck) 
                                               " загружен (" new-loaded " кг) и покинул центр")))
                      (do
                        (ui/log :loading (str "  Порт #" port-id ": грузовик частично загружен (" 
                                             new-loaded " / " (:capacity truck) " кг)"))
                        (<! (timeout 500))
                        (recur (- remaining load-weight) new-loaded)))))
                (do
                  (st/inc! :waiting-events)
                  (ui/log :waiting (str "  Порт #" port-id ": ожидание товара " (:category-name truck) " на складе..."))
                  (<! (timeout 1000))
                  (recur remaining loaded)))))
          
          (recur))))))

;; Генератор грузовиков на разгрузку
(defn start-truck-generator []
  (go-loop []
    (let [[_ ch] (alts! [(timeout 10000) control-chan])]
      (cond
        (= ch control-chan)
        (ui/log :info "Генератор грузовиков на разгрузку остановлен")
        
        :else
        (let [truck (generate-unloading-truck)]
          (ui/log :arrival (str "Прибыл грузовик на разгрузку: " (:id truck) 
                               " (" (:name truck) ", товаров: " (count (:items truck)) 
                               ", вес: " (:total-weight truck) " кг)"))
          (>! unloading-queue truck)
          (recur))))))

;; Генератор грузовиков на загрузку
(defn start-loading-truck-supplier []
  (go-loop []
    (let [[_ ch] (alts! [(timeout 3000) control-chan])]
      (cond
        (= ch control-chan)
        (ui/log :info "Генератор грузовиков на загрузку остановлен")
        
        :else
        (let [truck (generate-loading-truck)]
          (ui/log :arrival (str "Прибыл грузовик на загрузку: " (:id truck) 
                               " (" (:name truck) " для " (:category-name truck) ")"))
          (>! loading-queue truck)
          (recur))))))

;; Периодический вывод статуса
(defn start-status-monitor []
  (go-loop []
    (let [[_ ch] (alts! [(timeout 15000) control-chan])]
      (cond
        (= ch control-chan)
        (ui/log :info "Монитор статуса остановлен")
        
        :else
        (do
          (ui/log :warehouse "=== СОСТОЯНИЕ СКЛАДА ===")
          (if (empty? (wh/get-warehouse-state))
            (ui/log :warehouse "  Склад пуст")
            (doseq [[item-type categories] (wh/get-warehouse-state)
                    :when (seq categories)]
              (let [type-info (first (filter #(= (:type %) item-type) m/item-types))]
                (ui/log :warehouse (str "Тип: " (:name type-info)))
                (doseq [[cat weight] categories]
                  (let [cat-info (first (filter #(= (:id %) cat) (:categories type-info)))]
                    (ui/log :warehouse (str "  " (:name cat-info) ": " weight " кг")))))))
          (ui/log :warehouse "========================")
          
          (let [{:keys [unloaded-trucks loaded-trucks total-unloaded-weight total-loaded-weight]} (st/get-stats)]
            (ui/log :info (str "Статистика: разгружено " unloaded-trucks " грузовиков (" 
                              total-unloaded-weight " кг), загружено " loaded-trucks " грузовиков (" 
                              total-loaded-weight " кг)")))
          (recur))))))

(defn get-control-chan []
  control-chan)

(defn start-all []
  ;; Запуск 3 портов разгрузки
  (dotimes [i 3] (simulate-unloading-port (inc i)))
  
  ;; Запуск 5 портов загрузки
  (dotimes [i 5] (simulate-loading-port (inc i)))
  
  ;; Запуск генераторов
  (start-truck-generator)
  (start-loading-truck-supplier)
  (start-status-monitor))