(ns distribution-center.core
  (:require [clojure.core.async :as async :refer [go go-loop chan <! >! timeout close!]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [distribution-center.models :as m]
            [clojure.pprint :as pp])
  (:import [java.util.concurrent Executors TimeUnit]))

;; Форматирование времени
(def time-formatter (f/formatter "HH:mm:ss"))

(defn now-str []
  (f/unparse time-formatter (t/now)))

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

;; ===== ОЧЕРЕДИ =====
;; Очередь ожидания разгрузки (неограниченная)
(def unloading-queue (chan))

;; Порты разгрузки (3 шт) - буфер на 3 грузовика
(def unloading-ports (chan 3))

;; Порты загрузки (5 шт) - буфер на 5 грузовиков
(def loading-ports (chan 5))

;; ===== ЛОГГИРОВАНИЕ =====
(def color-map
  {:arrival    "\u001B[36m"   ; cyan - прибытие
   :unloading  "\u001B[32m"   ; green - разгрузка
   :loading    "\u001B[33m"   ; yellow - загрузка
   :warehouse  "\u001B[34m"   ; blue - склад
   :waiting    "\u001B[35m"   ; magenta - ожидание
   :departure  "\u001B[31m"   ; red - отъезд
   :reset      "\u001B[0m"})

(defn log [type msg]
  (println (str (color-map type) "[" (now-str) "] " msg (color-map :reset))))

;; ===== ГЕНЕРАЦИЯ ГРУЗОВИКОВ =====
(defn generate-items-for-truck [max-weight]
  "Генерация случайных товаров для грузовика на разгрузку"
  (loop [remaining max-weight
         items []]
    (if (<= remaining 1000) ; оставляем немного свободного места
      items
      (let [item-type (rand-nth m/item-types)
            category (rand-nth (:categories item-type))
            ;; Вес товара - случайное количество единиц
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
  "Генерация грузовика для разгрузки"
  (let [truck-type (rand-nth m/truck-types)
        items (generate-items-for-truck (:capacity truck-type))]
    {:id (str "U-" (System/currentTimeMillis))
     :type (:id truck-type)
     :name (:name truck-type)
     :capacity (:capacity truck-type)
     :items items
     :arrival-time (t/now)}))

(defn generate-loading-truck []
  "Генерация грузовика для загрузки (только малый и средний)"
  (let [truck-type (rand-nth (filter #(#{:small :medium} (:id %)) m/truck-types))
        ;; Выбираем случайный тип товара, но для пищевых - только пищевые категории
        item-type (rand-nth m/item-types)
        category (rand-nth (:categories item-type))]
    {:id (str "L-" (System/currentTimeMillis))
     :type (:id truck-type)
     :name (:name truck-type)
     :capacity (:capacity truck-type)
     :item-type (:type item-type)
     :category (:id category)
     :category-name (:name category)
     :loaded-weight 0
     :arrival-time (t/now)}))

;; ===== ПРОЦЕССЫ =====
(defn simulate-unloading-port [port-id]
  "Процесс разгрузки в одном порту"
  (go-loop []
    (when-let [truck (<! unloading-ports)]
      (log :unloading (str "Порт разгрузки #" port-id ": начало разгрузки грузовика " 
                          (:id truck) " (" (:name truck) ", " (count (:items truck)) " типов товаров)"))
      
      ;; Разгружаем каждый товар
      (doseq [item (:items truck)]
        (log :unloading (str "  Порт #" port-id ": разгрузка " (:units item) " ед. " 
                            (:category-name item) " (" (:weight item) " кг)"))
        (<! (timeout (* 100 (:time item)))) ; ускоряем для демонстрации (100 мс вместо 1 сек)
        (add-to-warehouse (:type item) (:category item) (:weight item))
        (log :warehouse (str "  Склад: добавлено " (:weight item) " кг " (:category-name item) 
                            " (всего: " (get-warehouse-stock (:type item) (:category item)) " кг)")))
      
      (log :departure (str "Порт разгрузки #" port-id ": грузовик " (:id truck) " разгружен и покинул центр"))
      (recur))))

(defn simulate-loading-port [port-id]
  "Процесс загрузки в одном порту"
  (go-loop []
    (when-let [truck (<! loading-ports)]
      (log :loading (str "Порт загрузки #" port-id ": начало загрузки грузовика " 
                        (:id truck) " (" (:name truck) " для " (:category-name truck) ")"))
      
      (loop [remaining-capacity (:capacity truck)]
        (let [available (get-warehouse-stock (:item-type truck) (:category truck))]
          (if (pos? available)
            (let [load-weight (min remaining-capacity available)
                  units (int (/ load-weight (:weight (first (filter #(= (:id %) (:category truck)) 
                                                                  (:categories (first (filter #(= (:type %) (:item-type truck)) m/item-types))))))))]
              ;; Загружаем товар
              (log :loading (str "  Порт #" port-id ": загрузка " units " ед. " (:category-name truck) 
                                " (" load-weight " кг)"))
              (<! (timeout (* 100 (int (/ load-weight 10))))) ; упрощённое время загрузки
              (remove-from-warehouse (:item-type truck) (:category truck) load-weight)
              (log :warehouse (str "  Склад: извлечено " load-weight " кг " (:category-name truck) 
                                  " (осталось: " (get-warehouse-stock (:item-type truck) (:category truck)) " кг)"))
              
              (if (>= (+ (:loaded-weight truck) load-weight) (:capacity truck))
                (log :departure (str "Порт загрузки #" port-id ": грузовик " (:id truck) 
                                    " загружен (" (+ (:loaded-weight truck) load-weight) " кг) и покинул центр"))
                (do
                  (log :loading (str "  Порт #" port-id ": грузовик частично загружен (" 
                                    (+ (:loaded-weight truck) load-weight) " / " (:capacity truck) " кг), ожидание товара..."))
                  (<! (timeout 500)) ; короткое ожидание перед следующей попыткой
                  (recur (- remaining-capacity load-weight)))))
            (do
              (log :waiting (str "  Порт #" port-id ": ожидание товара " (:category-name truck) " на складе..."))
              (<! (timeout 1000))
              (recur remaining-capacity)))))
      
      (recur))))

(defn truck-generator []
  "Генератор грузовиков на разгрузку (раз в 10 сек для демонстрации)"
  (go-loop []
    (<! (timeout 10000)) ; 10 секунд вместо минуты для удобства демонстрации
    (let [truck (generate-unloading-truck)]
      (log :arrival (str "Прибыл грузовик на разгрузку: " (:id truck) 
                        " (" (:name truck) ", товаров: " (count (:items truck)) ")"))
      
      ;; Проверяем наличие свободного порта
      (if (> (async/count unloading-ports) 0)
        (>! unloading-ports truck)
        (do
          (log :waiting (str "Нет свободных портов разгрузки, грузовик " (:id truck) " встал в очередь"))
          (>! unloading-queue truck)))
      (recur))))

(defn loading-truck-supplier []
  "Поставщик грузовиков на загрузку (постоянно)"
  (go-loop []
    (<! (timeout 3000)) ; каждые 3 секунды новый грузовик на загрузку
    (let [truck (generate-loading-truck)]
      (log :arrival (str "Прибыл грузовик на загрузку: " (:id truck) 
                        " (" (:name truck) " для " (:category-name truck) ")"))
      (>! loading-ports truck)
      (recur))))

(defn queue-manager []
  "Менеджер очереди ожидания разгрузки"
  (go-loop []
    (when-let [truck (<! unloading-queue)]
      ;; Ждём освобождения порта
      (<! unloading-ports)
      (log :unloading (str "Грузовик " (:id truck) " из очереди перемещён в порт разгрузки"))
      (>! unloading-ports truck))
    (recur)))

(defn print-status []
  "Периодическая печать статуса склада"
  (go-loop []
    (<! (timeout 15000))
    (log :warehouse (str "=== СОСТОЯНИЕ СКЛАДА ==="))
    (doseq [[item-type categories] @warehouse
            :when (seq categories)]
      (let [type-name (:name (first (filter #(= (:type %) item-type) m/item-types)))]
        (log :warehouse (str "Тип: " type-name))
        (doseq [[cat weight] categories]
          (let [cat-name (:name (first (filter #(= (:id %) cat) 
                                              (:categories (first (filter #(= (:type %) item-type) m/item-types))))))]
            (log :warehouse (str "  " cat-name ": " weight " кг"))))))
    (log :warehouse "========================")
    (recur)))

(defn start-simulation []
  "Запуск всей симуляции"
  (log :warehouse "Инициализация распределительного центра...")
  (log :warehouse "Конфигурация: 3 порта разгрузки, 5 портов загрузки")
  (log :warehouse "Типы грузовиков: малый (5т), средний (10т), большой (20т)")
  (log :warehouse "Типы товаров: крупногабаритные, среднегабаритные, малогабаритные, пищевые")
  (log :warehouse "Пищевые товары не смешиваются с другими типами")
  (println)
  
  ;; Запуск портов разгрузки
  (dotimes [i 3]
    (simulate-unloading-port (inc i)))
  
  ;; Запуск портов загрузки
  (dotimes [i 5]
    (simulate-loading-port (inc i)))
  
  ;; Запуск генераторов и менеджеров
  (truck-generator)
  (loading-truck-supplier)
  (queue-manager)
  (print-status)
  
  (log :warehouse "Симуляция запущена! Для остановки нажмите Ctrl+C")
  (log :warehouse "(Время ускорено для демонстрации: 1 сек = 10 сек реального времени)")
  (println))

(defn -main []
  (start-simulation)
  ;; Бесконечный цикл для удержания основного потока
  (loop []
    (Thread/sleep 1000)
    (recur)))