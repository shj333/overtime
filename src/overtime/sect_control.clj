(ns overtime.sect-control
  (:require [overtone.core :as ot]
            [overtime.instr-control :as instr]))


(defn- get-start-times
  [section-lengths]
  (let [session-start (+ (ot/now) 2000)
        lengths (take (dec (count section-lengths)) (reductions + (map #(* 1000 %) section-lengths)))
        start-times (cons 0 lengths)]
    (map #(+ session-start %) start-times)))

(defn- play-section
  [section-data opts]
  (let [{:keys [name events start-time]} section-data]
    (ot/apply-by start-time #(println "Starting section" name))
    (doseq [event-data events]
      (let [f (case (first event-data)
                :play instr/play-sound-at
                :stop instr/stop-sound
                :set instr/set-params-at
                :delta instr/set-param-over-time-at
                (println "Unknown control-func key" key))
            time (->> (second event-data)
                      (* 1000.0)
                      Math/round
                      (+ start-time))]
        (apply f time (rest (rest event-data)))))))


(defn play-sections
  ([sections-data] (play-sections sections-data {}))
  ([sections-data opts]
    ; TODO Add option to start at given section
    ; TODO Add option to skip section
   (let [start-times (get-start-times (map :length sections-data))
         sects-data-with-starts (map #(assoc %1 :start-time %2) sections-data start-times)]
     (doseq [section-data sects-data-with-starts] (play-section section-data opts)))))
