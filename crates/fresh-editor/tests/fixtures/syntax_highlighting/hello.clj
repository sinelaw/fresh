;; Clojure syntax highlighting test
(ns hello.core)

(defn greet [name]
  (str "Hello, " name "!"))

(def config {:version "1.0"
             :enabled true
             :count 42})

(defn -main [& args]
  (println (greet "World"))
  (doseq [item [1 2 3]]
    (println "Item:" item)))
