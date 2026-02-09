(ns distribution-center.core
  (:require [clojure.core.async :as async :refer [go go-loop chan <! >! timeout]]
            [distribution-center.models :as m])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

;; Форматирование времени через Java 8+ API
(def time-formatter (DateTimeFormatter/ofPattern "HH:mm:ss"))

(defn now-str []
  (.format (LocalDateTime/now) time-formatter))

;; ===== СКЛАД =====
;; Структура: {:large {:furniture 100, :electronics 50}, :food {:bread 200}, ...}
(def warehouse (atom {}))

(defn add-to-warehouse [item-type category weight]
  (swap! warehouse update-in [item-type category] 
         (fn [current] (if current (+ current weight) weight))))

(defn get-warehouse-stock [item-type category]
  (get-in @warehouse [item-type category] 0))

(defn remove-from-warehouse [item-type category weight]
  (let [current (get-warehouse-stock item-type category)
        new-val (- current weight)]
    (if (<= new-val 0)
      (swap! warehouse update item-type dissoc category)
      (swap! warehouse assoc-in [item-type category] new-val))
    (min weight current)))

;; ===== КАНАЛЫ =====
;; Очередь ожидания разгрузки
(def unloading-queue (chan))

;; Порты разгрузки (3 шт)
(def unloading-ports (chan 3))

;; Порты загрузки (5 шт)
(def loading-ports (chan 5))

;; ===== ЛОГГИРОВАНИЕ =====
(def color-map
  {:arrival    "\u001B[36m"   ; cyan
   :unloading  "\u001B[32m"   ; green
   :loading    "\u001B[33m"   ; yellow
   :warehouse  "\u001B[34m"   ; blue
   :waiting    "\u001B[35m"   ; magenta
   :departure  "\u001B[31m"   ; red
   :reset      "\u001B[0m"})

(defn log [type msg]
  (println (str (color-map type) "[" (now-str) "] " msg (color-map :reset))))

;; ===== ГЕНЕРАЦИЯ ГРУЗОВИКОВ =====
(defn generate-items-for-truck [max-weight]
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

(defn generate-unloading-truck []
  (let [truck-type (rand-nth m/truck-types)
        items (generate-items-for-truck (:capacity truck-type))]
    {:id (str "U-" (System/currentTimeMillis))
     :type (:id truck-type)
     :name (:name truck-type)
     :capacity (:capacity truck-type)
     :items items}))

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
     :category-name (:name category)
     :loaded-weight 0}))

;; ===== ПРОЦЕССЫ =====
(defn simulate-unloading-port [port-id]
  (go-loop []
    (when-let [truck (<! unloading-ports)]
      (log :unloading (str "Порт разгрузки #" port-id ": начало разгрузки грузовика " 
                          (:id truck) " (" (:name truck) ", товаров: " (count (:items truck)) ")"))
      
      (doseq [item (:items truck)]
        (log :unloading (str "  Порт #" port-id ": разгрузка " (:units item) " ед. " 
                            (:category-name item) " (" (:weight item) " кг)"))
        (<! (timeout (* 100 (:time item))))
        (add-to-warehouse (:type item) (:category item) (:weight item))
        (log :warehouse (str "  Склад: добавлено " (:weight item) " кг " (:category-name item) 
                            " (всего: " (get-warehouse-stock (:type item) (:category item)) " кг)")))
      
      (log :departure (str "Порт разгрузки #" port-id ": грузовик " (:id truck) " разгружен и покинул центр"))
      (recur))))

(defn simulate-loading-port [port-id]
  (go-loop []
    (when-let [truck (<! loading-ports)]
      (log :loading (str "Порт загрузки #" port-id ": начало загрузки грузовика " 
                        (:id truck) " (" (:name truck) " для " (:category-name truck) ")"))
      
      (loop [remaining-capacity (:capacity truck)]
        (let [available (get-warehouse-stock (:item-type truck) (:category truck))]
          (if (pos? available)
            (let [load-weight (min remaining-capacity available)
                  category-info (first (filter #(= (:id %) (:category truck)) 
                                              (:categories (first (filter #(= (:type %) (:item-type truck)) m/item-types))))) 
                  units (int (/ load-weight (:weight category-info)))]
              (log :loading (str "  Порт #" port-id ": загрузка " units " ед. " (:category-name truck) 
                                " (" load-weight " кг)"))
              (<! (timeout (* 100 (max 1 (int (/ load-weight 10))))))
              (remove-from-warehouse (:item-type truck) (:category truck) load-weight)
              (log :warehouse (str "  Склад: извлечено " load-weight " кг " (:category-name truck) 
                                  " (осталось: " (get-warehouse-stock (:item-type truck) (:category truck)) " кг)"))
              
              (if (>= (+ (:loaded-weight truck) load-weight) (:capacity truck))
                (log :departure (str "Порт загрузки #" port-id ": грузовик " (:id truck) 
                                    " загружен (" (+ (:loaded-weight truck) load-weight) " кг) и покинул центр"))
                (do
                  (log :loading (str "  Порт #" port-id ": грузовик частично загружен (" 
                                    (+ (:loaded-weight truck) load-weight) " / " (:capacity truck) " кг)"))
                  (<! (timeout 500))
                  (recur (- remaining-capacity load-weight)))))
            (do
              (log :waiting (str "  Порт #" port-id ": ожидание товара " (:category-name truck) " на складе..."))
              (<! (timeout 1000))
              (recur remaining-capacity)))))
      
      (recur))))

(defn truck-generator []
  (go-loop []
    (<! (timeout 10000)) ; каждые 10 секунд
    (let [truck (generate-unloading-truck)]
      (log :arrival (str "Прибыл грузовик на разгрузку: " (:id truck) 
                        " (" (:name truck) ", товаров: " (count (:items truck)) ")"))
      
      (if (> (async/count unloading-ports) 0)
        (>! unloading-ports truck)
        (do
          (log :waiting (str "Нет свободных портов разгрузки, грузовик " (:id truck) " встал в очередь"))
          (>! unloading-queue truck)))
      (recur))))

(defn loading-truck-supplier []
  (go-loop []
    (<! (timeout 3000)) ; каждые 3 секунды
    (let [truck (generate-loading-truck)]
      (log :arrival (str "Прибыл грузовик на загрузку: " (:id truck) 
                        " (" (:name truck) " для " (:category-name truck) ")"))
      (>! loading-ports truck)
      (recur))))

(defn queue-manager []
  (go-loop []
    (when-let [truck (<! unloading-queue)]
      (<! unloading-ports)
      (log :unloading (str "Грузовик " (:id truck) " из очереди перемещён в порт разгрузки"))
      (>! unloading-ports truck))
    (recur)))

(defn print-status []
  (go-loop []
    (<! (timeout 15000))
    (log :warehouse "=== СОСТОЯНИЕ СКЛАДА ===")
    (doseq [[item-type categories] @warehouse
            :when (seq categories)]
      (let [type-info (first (filter #(= (:type %) item-type) m/item-types))]
        (log :warehouse (str "Тип: " (:name type-info)))
        (doseq [[cat weight] categories]
          (let [cat-info (first (filter #(= (:id %) cat) (:categories type-info)))]
            (log :warehouse (str "  " (:name cat-info) ": " weight " кг"))))))
    (log :warehouse "========================")
    (recur)))

(defn start-simulation []
  (println (str (color-map :warehouse) "╔══════════════════════════════════════════════════════════════╗" (color-map :reset)))
  (println (str (color-map :warehouse) "║  СИМУЛЯТОР РАСПРЕДЕЛИТЕЛЬНОГО ЦЕНТРА                      ║" (color-map :reset)))
  (println (str (color-map :warehouse) "╠══════════════════════════════════════════════════════════════╣" (color-map :reset)))
  (println (str (color-map :warehouse) "║  Конфигурация: 3 порта разгрузки, 5 портов загрузки      ║" (color-map :reset)))
  (println (str (color-map :warehouse) "║  Грузовики: малый(5т), средний(10т), большой(20т)         ║" (color-map :reset)))
  (println (str (color-map :warehouse) "║  Товары: крупногабаритные, среднегабаритные,              ║" (color-map :reset)))
  (println (str (color-map :warehouse) "║          малогабаритные, пищевые                          ║" (color-map :reset)))
  (println (str (color-map :warehouse) "║  Пищевые товары НЕ смешиваются с другими типами           ║" (color-map :reset)))
  (println (str (color-map :warehouse) "╚══════════════════════════════════════════════════════════════╝" (color-map :reset)))
  (println)
  
  ;; Запуск портов
  (dotimes [i 3] (simulate-unloading-port (inc i)))
  (dotimes [i 5] (simulate-loading-port (inc i)))
  
  ;; Запуск менеджеров
  (truck-generator)
  (loading-truck-supplier)
  (queue-manager)
  (print-status)
  
  (log :warehouse "Симуляция запущена! Для остановки нажмите Ctrl+C")
  (log :warehouse "(Время ускорено: генерация грузовиков каждые 10 сек вместо 1 мин)")
  (println))

(defn -main []
  (start-simulation)
  (loop [] (Thread/sleep 1000) (recur)))