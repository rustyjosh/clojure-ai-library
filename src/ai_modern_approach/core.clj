(ns ai-modern-approach.core)

(defn repeat-move? 
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
  [ states_so_far current_step ]
  ( let [[right_rabbits right_cabbage left_rabbits left_cabbage] 
         [(first current_step)
          (last current_step)
          ( - ( first (first states_so_far)) (first current_step ))
          ( - ( last (first states_so_far)) (last current_step ))
         ]]
    ( cond
      ( > 0 right_rabbits ) false
      ( > 0 right_cabbage ) false
      ( > 0 left_rabbits ) false
      ( > 0  left_cabbage ) false      
      ( and ( > left_rabbits left_cabbage ) ( not= 0 left_cabbage ) ) false
      ( and ( > right_rabbits right_cabbage ) ( not= 0 right_cabbage ) ) false
      :else true
    ) 
  )
)

(defn is_move_allowed_and_new?
  [states_so_far current_step]
  (filterv 
    (apply every-pred 
      [(partial is_move_allowed? states_so_far) (partial repeat-move? states_so_far)]) 
    current_step
  )
)
           


(defn generate_moves
  { :doc "Generates all legal moves from the passed state" }
  [states_so_far]
  (let [current_step ( last states_so_far )]
    (is_move_allowed_and_new? 
        states_so_far
        (mapv 
            #(vector 
              (- (first current_step) (first %)) 
              (- (last current_step) (last %))
            )[[0 1] [0 2] [1 1] [2 0] [1 0]]
        )
    )
  )
)


(defn explore_node
  { :doc "Checks whether a node is a solution or not. If not, generates further states." }
  [states_so_far solutions_so_far]
  ( if ( is_solution? ( last states_so_far ))
    (println (conj solutions_so_far states_so_far))
    (doseq [[next_rabbit next_cabbage] (generate_moves states_so_far)]
      ( conj solutions_so_far (explore_node (conj states_so_far [next_rabbit next_cabbage]) solutions_so_far))
    )
  )
)


(defn -main
  { :doc "Main" }
  []
  (println "Number of rabbits?")
  (let [rabbits (read-line) cabbages (read-line)] 
    (println "All solutions: " (explore_node [[(read-string rabbits) (read-string cabbages)]] []))
  )
)
