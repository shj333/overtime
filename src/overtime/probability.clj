(ns overtime.probability)


(defn exp-rand
  [low high]
  (* low (Math/exp (* (Math/log (/ high low)) (rand)))))