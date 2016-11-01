(ns overtime.instruments
  (:require [overtone.core :as ot]
            [overtime.sounds :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]
            [overtime.patterns :as pat]))


; TODO create new namespace to isolate overtone function calls

(defonce ^:private instrs (atom {}))


(defn- instr [key] (u/check-nil (@instrs key) "Unknown synth key" key))

(defn- get-sound-params
  [params]
  (flatten (for [[key val] (partition 2 params)] [key (snd/sound-param key val)])))

(defn- set-params
  [time instr-key params]
  (let [sound-params (get-sound-params params)]
    (log/debug "Set params for" instr-key "to" sound-params)
    (ot/at time (apply ot/ctl (instr instr-key) sound-params))))

(defn- get-delta-vals
  [start-val num-steps f]
  (->> start-val
       (iterate f)
       (take num-steps)))

(defn- set-param-over-time
  [instr-key param-key num-steps val-delta-f time-delta-f]
  (let [synth (instr instr-key)
        start-val (ot/node-get-control synth param-key)
        vals (get-delta-vals start-val num-steps val-delta-f)
        times (get-delta-vals (+ (ot/now) 500) num-steps time-delta-f)
        vals-times (map vector vals times)]
    (log/debug "Vals:" vals)
    (log/debug "Times:" times)
    (doseq [[val time] vals-times] (set-params time instr-key [param-key val]))
    [synth vals-times]))

(declare add-instrs)

(defn- play-instr
  [time instr-key sound-def-key]
  (let [{:keys [synth params]} (snd/sound-def sound-def-key)]
    (ot/at time
           (->> (apply (u/check-nil synth "Synth" instr-key) params)
                (hash-map instr-key)
                add-instrs))))

(defn- stop-instr
  ([instr-key] (stop-instr instr-key 10))
  ([instr-key num-incrs]
   (log/debug "Stopping instr" instr-key "over" num-incrs "increments")
   (let [time-delta 250
         [synth vals-times] (set-param-over-time instr-key :amp num-incrs #(* % 0.50) #(+ % time-delta))
         last-time (-> vals-times last second)]
     (log/debug "Last time:" last-time)
     (ot/at (+ time-delta last-time) (ot/kill synth)))))



; Public API
(defn add-instrs
  "Add given key value pairs representing synth instruments to atom that manages their state"
  [new-instrs]
  (swap! instrs merge new-instrs))

(defn kill-sound
  "Convenience method to kill instrument sound identified by given key"
  [instr-key]
  (ot/kill (instr instr-key)))



; Sound event handling
(defn handle-event [time event-data] (pat/handle-event time event-data))

; TODO These can be handled by a macro to keep things DRY
(defmethod pat/handle-event :play-instr
  [time [_event-type instr-key sound-def-key-p]]
  (let [sound-def-key (or sound-def-key-p instr-key)]
    (u/apply-by time
                (do
                  (log/info "Playing instr" instr-key "using sound def" sound-def-key)
                  (play-instr time instr-key sound-def-key))))
  true)

(defmethod pat/handle-event :stop-instr
  [time [_event-type instr-key]]
  (u/apply-by time
              (do
                (log/info "Stopping instr" instr-key)
                (stop-instr instr-key)))
  true)

(defmethod pat/handle-event :set-instr
  [time [_event-type instr-key & params]]
  (u/apply-by time
              (do
                (log/info "Setting params" (u/print-param-keys params) "for" instr-key)
                (set-params time instr-key params)))
  true)

(defmethod pat/handle-event :delta-instr
  [time [_event-type instr-key param-key num-steps val-delta-f time-delta-f]]
  (u/apply-by time
              (do
                (log/info "Setting param" param-key "for" instr-key "over" num-steps "steps")
                (set-param-over-time instr-key param-key num-steps val-delta-f time-delta-f)))
  true)
