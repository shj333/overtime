(ns overtime.sect-control
  (:require [overtone.core :as ot]
            [clojure.tools.logging :as log]))


(defmulti instr-control-f (fn [event-data] (first event-data)))
(defmethod instr-control-f :default [event-data] (log/error "Unknown control-func key" (first event-data)))

(defn- get-start-times
  [section-lengths]
  (let [session-start (+ (ot/now) 2000)
        lengths (take (dec (count section-lengths)) (reductions + (map #(* 1000 %) section-lengths)))
        start-times (cons 0 lengths)]
    (map #(+ session-start %) start-times)))

(defn- event-time
  [start-time event-data]
  (->> (second event-data)
       (* 1000.0)
       Math/round
       (+ start-time)))

(defn- play-section
  [section-data opts]
  (let [{:keys [name events start-time]} section-data]
    (ot/apply-by start-time #(log/info "Starting section" name))
    (doseq [event-data events]
      (let [f (instr-control-f event-data)
            time (event-time start-time event-data)]
        (apply f time (rest (rest event-data)))))))


(defn play-sections
  ([sections-data] (play-sections sections-data {}))
  ([sections-data opts]
    ; TODO Add option to start at given section
    ; TODO Add option to skip section
   (let [start-times (get-start-times (map :length sections-data))
         sects-data-with-starts (map #(assoc %1 :start-time %2) sections-data start-times)]
     (doseq [section-data sects-data-with-starts] (play-section section-data opts)))))
