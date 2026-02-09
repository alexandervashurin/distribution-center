(ns distribution-center.models)

;; Типы грузовиков (3 типа с разной грузоподъёмностью)
(def truck-types
  [{:id :small  :name "Малый"   :capacity 5000}   ; 5 тонн
   {:id :medium :name "Средний" :capacity 10000}  ; 10 тонн
   {:id :large  :name "Большой" :capacity 20000}]) ; 20 тонн

;; Типы товаров с подкатегориями
;; Вес в кг, время обработки в секундах (для симуляции)
(def item-types
  [{:type :large  :name "Крупногабаритные" :categories [{:id :furniture :name "Мебель" :weight 50 :time 10}
                                                        {:id :electronics :name "Электроника" :weight 30 :time 8}]}
   {:type :medium :name "Среднегабаритные" :categories [{:id :clothing :name "Одежда" :weight 5 :time 3}
                                                         {:id :tools :name "Инструменты" :weight 10 :time 5}]}
   {:type :small  :name "Малогабаритные"  :categories [{:id :toys :name "Игрушки" :weight 1 :time 2}
                                                         {:id :books :name "Книги" :weight 2 :time 2}]}
   {:type :food   :name "Пищевые"         :categories [{:id :bread :name "Хлеб" :weight 10 :time 4}
                                                        {:id :milk :name "Молоко" :weight 15 :time 5}
                                                        {:id :potato :name "Картофель" :weight 20 :time 6}]}])