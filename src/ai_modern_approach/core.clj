(ns ai-modern-approach.core)

(defn samplefunction 
  { :doc "I don't do a whole lot."}
  []
  (println "Hello, World!")
  (println ( + 1 1 ))
  ( + 1 1 )
)

(defn -main
  "Main"
  []
  ( def x (samplefunction))
  ( println (str x "ez2win"))
)

