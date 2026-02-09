(ns distribution-center.warehouse)

;; Склад хранит товары по типам и категориям
;; Структура: {:large {:furniture 100, :electronics 50}, :food {:bread 200}, ...}
(def warehouse (atom {}))

(defn add-item [item-type category weight]
  (swap! warehouse update-in [item-type category] 
         (fn [current] (if current (+ current weight) weight))))

(defn get-stock [item-type category]
  (get-in @warehouse [item-type category] 0))

(defn remove-item [item-type category weight]
  (let [current (get-stock item-type category)
        new-val (- current weight)]
    (if (<= new-val 0)
      (swap! warehouse update item-type dissoc category)
      (swap! warehouse assoc-in [item-type category] new-val))
    (min weight current)))

(defn get-warehouse-state []
  @warehouse)