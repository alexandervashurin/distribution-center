(ns distribution-center.core
  (:require [distribution-center.ui :as ui]
            [distribution-center.simulation :as sim]
            [distribution-center.stats :as st]
            [distribution-center.warehouse :as wh])
  (:import [java.lang Runtime]))

(defn print-current-stats []
  (st/print-final-stats (wh/get-warehouse-state)))

(defn graceful-shutdown []
  (ui/log :info "Начало корректного завершения...")
  
  ;; Отправляем сигнал остановки
  (when-not (clojure.core.async/poll! (sim/get-control-chan))
    (clojure.core.async/>!! (sim/get-control-chan) :stop))
  (clojure.core.async/close! (sim/get-control-chan))
  
  ;; Даём время на завершение
  (Thread/sleep 2000)
  
  ;; Выводим финальную статистику
  (print-current-stats)
  
  (ui/log :info "Симуляция завершена. До свидания!")
  (System/exit 0))

(defn -main []
  (ui/print-banner)
  
  ;; Запуск симуляции
  (sim/start-all)
  
  ;; Запуск слушателя клавиш
  (ui/keyboard-listener (sim/get-control-chan) print-current-stats)
  
  ;; Регистрация обработчика завершения (Ctrl+C)
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. graceful-shutdown))
  
  (ui/log :info "Симуляция запущена!")
  (ui/log :info "Управление: нажмите 'q' + Enter для остановки, 's' + Enter для вывода статистики")
  (ui/log :info "(Время ускорено: генерация грузовиков каждые 10 сек вместо 1 мин)")
  
  ;; Основной поток ждёт команды остановки
  (loop []
    (when-not (clojure.core.async/poll! (sim/get-control-chan))
      (Thread/sleep 100)
      (recur)))
  (graceful-shutdown))