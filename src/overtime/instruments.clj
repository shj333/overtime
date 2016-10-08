(ns overtime.instruments
  (:require [overtone.core :as ot]
            [overtime.microsounds :as micro]
            [overtime.sounds :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private instrs (atom {}))


(defn instr [instr-key] (u/check-nil (@instrs instr-key) "Instr" instr-key))

(defmulti synth-instance (fn [type _key] type))
(defmethod synth-instance :default [_type _key] (log/error "Unknown synth type" type))
(defmethod synth-instance :instr [_type key] (instr key))
(defmethod synth-instance :trigger [_type key] (micro/trigger key))
(defmethod synth-instance :pan [_type key] (micro/pan key))

(defmulti set-params (fn [type _key & _params] type))
(defmethod set-params :default
  [type key & params]
  (let [sound-params (flatten (for [[key val] (partition 2 params)] [key (snd/sound-param key val)]))]
    (log/debug "Set params for" type key "to" sound-params)
    (apply ot/ctl (synth-instance type key) sound-params)))

(defn set-params-at
  [time type key & params]
  (u/apply-by time
              (do
                (log/info "Set params for" type key ", params" (take-nth 2 params))
                (ot/at time (apply set-params type key params)))))

(defn- get-delta-vals
  [start-val num-steps f]
  (->> start-val
       (iterate f)
       (take num-steps)))

(defn set-param-over-time
  [type key param-key num-steps val-delta-f time-delta-f]
  (log/info "Setting param" param-key "for" type key "over" num-steps "steps")
  (let [synth (synth-instance type key)
        start-val (ot/node-get-control synth param-key)
        vals (get-delta-vals start-val num-steps val-delta-f)
        times (get-delta-vals (+ (ot/now) 500) num-steps time-delta-f)
        vals-times (map vector vals times)]
    (log/debug "Vals:" vals)
    (log/debug "Times:" times)
    (doseq [[val time] vals-times] (ot/at time (set-params type key param-key val)))
    [synth vals-times]))

(defn set-param-over-time-at [time & params] (u/apply-by time (apply set-param-over-time params)))


(defn play-instr
  [instr-key synth params]
  (log/info "Playing instr" instr-key)
  (swap! instrs assoc instr-key (-> (u/check-nil synth "Synth" instr-key)
                                    (apply params))))

(defn play-sound
  [instr-key sound-def-key]
  (let [{:keys [synth params]} (snd/sound-def sound-def-key)]
    (play-instr instr-key synth params)))

(defmulti play-sound-at (fn [_time instr-type & _args] instr-type))
(defmethod play-sound-at :instr
  [time _instr-type & [instr-key sound-def-key]]
  (u/apply-by time (ot/at time (play-sound instr-key (or sound-def-key instr-key)))))

(defn stop-instr
  ([instr-key] (stop-instr instr-key 10))
  ([instr-key num-incrs]
   (log/info "Stopping instr" instr-key "over" num-incrs "increments")
   (let [time-delta 250
         [synth vals-times] (set-param-over-time :instr instr-key :amp num-incrs #(* % 0.50) #(+ % time-delta))
         last-time (-> vals-times last second)]
     (log/debug "Last time:" last-time)
     (ot/at (+ time-delta last-time) (ot/kill synth)))))

(defmulti stop-sound-at (fn [_time instr-type & _instr-key] instr-type))
(defmethod stop-sound-at :instr
  [time _instr-type instr-key]
  (u/apply-by time (stop-instr instr-key)))

(defn kill-sound
  [instr-key]
  (ot/kill (instr instr-key)))


(defmulti instr-control-f (fn [event-data] (first event-data)))
(defmethod instr-control-f :default [event-data] (log/error "Unknown control-func key" (first event-data)))
(defmethod instr-control-f :play [_event-data] play-sound-at)
(defmethod instr-control-f :stop [_event-data] stop-sound-at)
(defmethod instr-control-f :set [_event-data] set-params-at)
(defmethod instr-control-f :delta [_event-data] set-param-over-time-at)
