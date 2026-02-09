(ns distribution-center.core
  (:require [clojure.core.async :as async]
            [distribution-center.models :as models]))

;; Склад хранит товары по типам и категориям
(def warehouse (atom {}))

;; Генерация грузовика для разгрузки
(defn generate-unloading-truck []
  (let [truck (rand-nth models/truck-types)
        max-weight (:capacity truck)
        items (loop [remaining-weight max-weight
                     items []]
                (if (<= remaining-weight 0)
                  items
                  (let [item-type (rand-nth models/item-types)
                        category (rand-nth (:categories item-type))
                        weight (inc (rand-int (min 5 (dec remaining-weight))))
                        unload-time (inc (rand-int 5))]
                    (recur (- remaining-weight weight)
                           (conj items {:type (:type item-type)
                                        :category category
                                        :weight weight
                                        :unload-time unload-time})))))]
    {:truck truck
     :items items}))

;; Генерация грузовика для загрузки
(defn generate-loading-truck []
  (let [item-type (rand-nth models/item-types)
        category (rand-nth (:categories item-type))
        truck (rand-nth (filter #(not= (:id %) :large) models/truck-types))]
    {:truck truck
     :item-type (:type item-type)
     :category category}))

;; Операции со складом
(defn add-to-warehouse [item]
  (swap! warehouse update-in [(:type item) (:category item)]
          (fn [current] (if current (+ current (:weight item)) (:weight item)))))

(defn remove-from-warehouse [item-type category weight]
  (swap! warehouse update-in [item-type category]
          (fn [current] (- current weight))))

;; Разгрузка грузовика
(defn unload-truck [truck]
  (async/go
    (println (str "Starting unloading truck " (:id (:truck truck))))
    (doseq [item (:items truck)]
      (async/<! (async/timeout (* 1000 (:unload-time item))))
      (add-to-warehouse item)
      (println (str "Added " (:weight item) " of " (:category item) " to warehouse")))
    (println "Truck unloaded")))

;; Загрузка грузовика
(defn load-truck [truck]
  (async/go
    (loop [remaining-capacity (:capacity (:truck truck))]
      (let [item-type (:item-type truck)
            category (:category truck)
            current-weight (get-in @warehouse [item-type category] 0)
            load-weight (min current-weight remaining-capacity)]
        (if (pos? load-weight)
          (do
            (remove-from-warehouse item-type category load-weight)
            (async/<! (async/timeout (* 1000 load-weight)))
            (println (str "Loaded " load-weight " of " category " into truck"))
            (if (pos? (- remaining-capacity load-weight))
              (recur (- remaining-capacity load-weight))
              (println "Truck fully loaded")))
          (do
            (println (str "Waiting for " category " on warehouse..."))
            (async/<! (async/timeout 1000))
            (recur remaining-capacity)))))))

;; Запуск симуляции
(defn start-simulation []
  (let [unloading-channel (async/chan 3)  ;; 3 порта разгрузки
        loading-channel (async/chan 5)]    ;; 5 портов загрузки
    
    ;; Порты разгрузки
    (dotimes [i 3]
      (async/go
        (loop []
          (let [truck (async/<! unloading-channel)]
            (unload-truck truck)
            (recur)))))
    
    ;; Порты загрузки
    (dotimes [i 5]
      (async/go
        (loop []
          (let [truck (async/<! loading-channel)]
            (load-truck truck)
            (recur)))))
    
    ;; Генератор грузовиков для разгрузки (каждую минуту)
    (async/go
      (loop []
        (async/<! (async/timeout (* 60 1000)))
        (let [truck (generate-unloading-truck)]
          (println (str "Generated truck for unloading: " (:id (:truck truck))))
          (async/>! unloading-channel truck))
        (recur)))
    
    ;; Генератор грузовиков для загрузки (постоянно)
    (async/go
      (loop []
        (let [truck (generate-loading-truck)]
          (println (str "Generated truck for loading: " (:id (:truck truck))))
          (async/>! loading-channel truck))
        (recur)))
    
    (println "Distribution center simulation started...")))

(defn -main []
  (start-simulation)
  (Thread/sleep Long/MAX_VALUE))