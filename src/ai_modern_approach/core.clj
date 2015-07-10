(ns ai-modern-approach.core)

(defn not-in? 
  { :doc "Test if an array does not contain an item"}
  [array item_to_search_for]  
  (not-any? #{[item_to_search_for]} [array])
)

(defn is_solution?
  { :doc "Returns true if the left side of the river has no rabbits or cabbages." }
  [state]
  ( if ( and ( == 0 ( first state ) ) ( == 0 ( last state ) ) ) 
    true
    false
  )
)


(defn is_move_allowed?
  { :doc "Given the number of rabbits and cabbages on one side of the river, validates both sides." }
  [ state ]
  ( let [[rabbits cabbage] [(first state) (last state)]]
    ( cond
      ( > ( - 5 rabbits ) ( - 6 cabbage ) ) false
      ( > rabbits cabbage ) false
      ( > 0 rabbits ) false
      ( > 0 cabbage ) false
      :else true
    ) 
  )
)


(defn generate_moves
  { :doc "Generates all legal moves from the passed state" }
  [states_so_far]
  ( let [current_step ( last states_so_far )]
    (filterv is_move_allowed? (filterv #(not-in? states_so_far %) (mapv #(vector (- (first current_step) (first %)) (- (last current_step) (last %))) [[0 1] [0 2] [1 1] [2 0] [1 0]])))
  )
)


(defn explore_node
  { :doc "Checks whether a node is a solution or not. If not, generates further states." }
  [states_so_far]
  ( if ( is_solution? ( last states_so_far ))
    (println "Solution: " [states_so_far])
    (doseq [[next_rabbit next_cabbage] (generate_moves states_so_far)]
      (explore_node (conj states_so_far [next_rabbit next_cabbage]))
    )
  )
)


(defn -main
  { :doc "Main" }
  []
  (explore_node [[5 6]])
)
