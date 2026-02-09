(defproject distribution-center "0.1.0-SNAPSHOT"
  :description "Симулятор распределительного центра"
  :url "https://github.com/alexandervashurin/distribution-center"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.5.648"]
                 [clj-time "0.15.2"]
                 [com.cemerick/puget "1.2.1"]] ; для красивого вывода
  :main ^:skip-aot distribution-center.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})