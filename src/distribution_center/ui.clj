(ns distribution-center.ui
  (:require [clojure.core.async :as async :refer [>!! close!]])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.io BufferedReader InputStreamReader]))

;; Форматирование времени
(def time-formatter (DateTimeFormatter/ofPattern "HH:mm:ss"))

(defn now-str []
  (.format (LocalDateTime/now) time-formatter))

;; Цвета для логов
(def color-map
  {:arrival    "\u001B[36m"   ; cyan
   :unloading  "\u001B[32m"   ; green
   :loading    "\u001B[33m"   ; yellow
   :warehouse  "\u001B[34m"   ; blue
   :waiting    "\u001B[35m"   ; magenta
   :departure  "\u001B[31m"   ; red
   :info       "\u001B[37m"   ; white
   :reset      "\u001B[0m"})

(defn log [type msg]
  (println (str (color-map type) "[" (now-str) "] " msg (color-map :reset))))

(defn print-banner []
  (println (str (color-map :warehouse) "╔══════════════════════════════════════════════════════════════╗" (color-map :reset)))
  (println (str (color-map :warehouse) "║  СИМУЛЯТОР РАСПРЕДЕЛИТЕЛЬНОГО ЦЕНТРА                      ║" (color-map :reset)))
  (println (str (color-map :warehouse) "╠══════════════════════════════════════════════════════════════╣" (color-map :reset)))
  (println (str (color-map :warehouse) "║  Конфигурация: 3 порта разгрузки, 5 портов загрузки      ║" (color-map :reset)))
  (println (str (color-map :warehouse) "║  Грузовики: малый(5т), средний(10т), большой(20т)         ║" (color-map :reset)))
  (println (str (color-map :warehouse) "║  Товары: крупногабаритные, среднегабаритные,              ║" (color-map :reset)))
  (println (str (color-map :warehouse) "║          малогабаритные, пищевые                          ║" (color-map :reset)))
  (println (str (color-map :warehouse) "║  Пищевые товары НЕ смешиваются с другими типами           ║" (color-map :reset)))
  (println (str (color-map :warehouse) "╚══════════════════════════════════════════════════════════════╝" (color-map :reset)))
  (println))

(defn keyboard-listener [control-chan print-stats-fn]
  (log :info "Управление: нажмите 'q' для остановки симуляции, 's' для вывода статистики")
  (let [reader (BufferedReader. (InputStreamReader. System/in))]
    (.start (Thread.
             (fn []
               (try
                 (loop []
                   (when-let [line (.readLine reader)]
                     (case (.toLowerCase line)
                       "q" (do
                             (log :info "Получена команда остановки. Завершение симуляции...")
                             (>!! control-chan :stop)
                             (close! control-chan))
                       "s" (print-stats-fn)
                       (log :info (str "Неизвестная команда: '" line "'. Доступны: q (стоп), s (статистика)")))
                     (when-not (= (.toLowerCase line) "q")
                       (recur))))
                 (catch Exception e
                   (log :info (str "Ошибка слушателя клавиш: " (.getMessage e))))))))))
