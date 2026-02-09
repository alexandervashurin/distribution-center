(ns distribution-center.models)

;; Типы грузовиков (3 типа)
(def truck-types
  [{:id :small :capacity 10}
   {:id :medium :capacity 20}
   {:id :large :capacity 30}])

;; Типы товаров с подкатегориями
(def item-types
  [{:type :large :categories [:furniture :electronics]}
   {:type :medium :categories [:clothing :tools]}
   {:type :small :categories [:toys :books]}
   {:type :food :categories [:bread :milk :potato]}])