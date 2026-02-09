(ns distribution-center.stats
  (:require [distribution-center.models :as m])
  (:import [java.time LocalDateTime]))

;; Статистика симуляции
(def stats (atom {:start-time (LocalDateTime/now)
                  :unloaded-trucks 0
                  :loaded-trucks 0
                  :total-unloaded-weight 0
                  :total-loaded-weight 0
                  :warehouse-operations 0
                  :waiting-events 0}))

(defn inc! [key & [amount]]
  (swap! stats update key + (or amount 1)))

(defn get-stats []
  @stats)

(defn print-final-stats [warehouse-state]
  (let [{:keys [start-time unloaded-trucks loaded-trucks 
                total-unloaded-weight total-loaded-weight
                warehouse-operations waiting-events]} (get-stats)
        end-time (LocalDateTime/now)
        duration-seconds (.getSeconds (.between start-time end-time))
        color-map {:info "\u001B[37m" :reset "\u001B[0m"}]
    
    (println)
    (println (str (color-map :info) "╔══════════════════════════════════════════════════════════════╗" (color-map :reset)))
    (println (str (color-map :info) "║          ФИНАЛЬНАЯ СТАТИСТИКА СИМУЛЯЦИИ                    ║" (color-map :reset)))
    (println (str (color-map :info) "╠══════════════════════════════════════════════════════════════╣" (color-map :reset)))
    (println (str (color-map :info) "║ Время работы: " (format "%4d сек (%.1f мин)" duration-seconds (/ duration-seconds 60.0)) 
                  (apply str (repeat (- 45 (count (format "%4d сек (%.1f мин)" duration-seconds))) " ")) "║" (color-map :reset)))
    (println (str (color-map :info) "╠══════════════════════════════════════════════════════════════╣" (color-map :reset)))
    (println (str (color-map :info) "║ Разгружено грузовиков: " (format "%5d" unloaded-trucks) 
                  (apply str (repeat (- 38 (count (str unloaded-trucks))) " ")) "║" (color-map :reset)))
    (println (str (color-map :info) "║ Общий вес разгрузки:   " (format "%7d кг" total-unloaded-weight) 
                  (apply str (repeat (- 36 (count (str total-unloaded-weight))) " ")) "║" (color-map :reset)))
    (println (str (color-map :info) "╠══════════════════════════════════════════════════════════════╣" (color-map :reset)))
    (println (str (color-map :info) "║ Загружено грузовиков:  " (format "%5d" loaded-trucks) 
                  (apply str (repeat (- 38 (count (str loaded-trucks))) " ")) "║" (color-map :reset)))
    (println (str (color-map :info) "║ Общий вес загрузки:    " (format "%7d кг" total-loaded-weight) 
                  (apply str (repeat (- 36 (count (str total-loaded-weight))) " ")) "║" (color-map :reset)))
    (println (str (color-map :info) "╠══════════════════════════════════════════════════════════════╣" (color-map :reset)))
    (println (str (color-map :info) "║ Операций со складом:   " (format "%5d" warehouse-operations) 
                  (apply str (repeat (- 38 (count (str warehouse-operations))) " ")) "║" (color-map :reset)))
    (println (str (color-map :info) "║ Событий ожидания:      " (format "%5d" waiting-events) 
                  (apply str (repeat (- 38 (count (str waiting-events))) " ")) "║" (color-map :reset)))
    (println (str (color-map :info) "╠══════════════════════════════════════════════════════════════╣" (color-map :reset)))
    
    ;; Состояние склада
    (println (str (color-map :info) "║ СКЛАД (остатки):                                           ║" (color-map :reset)))
    (if (empty? warehouse-state)
      (println (str (color-map :info) "║   Пусто                                                    ║" (color-map :reset)))
      (doseq [[item-type categories] warehouse-state
              :when (seq categories)]
        (let [type-info (first (filter #(= (:type %) item-type) m/item-types))]
          (println (str (color-map :info) "║   " (:name type-info) 
                        (apply str (repeat (- 56 (count (:name type-info))) " ")) "║" (color-map :reset)))
          (doseq [[cat weight] categories]
            (let [cat-info (first (filter #(= (:id %) cat) (:categories type-info)))]
              (println (str (color-map :info) "║     " (:name cat-info) ": " weight " кг"
                            (apply str (repeat (- 49 (count (str (:name cat-info) ": " weight " кг"))) " ")) "║" (color-map :reset))))))))
    
    (println (str (color-map :info) "╚══════════════════════════════════════════════════════════════╝" (color-map :reset)))
    (println)))