(ns overtime.instr-control
  (:require [overtone.core :as ot]
            [overtime.microsound :as micro]
            [overtime.sound-control :as snd]
            [overtime.utils :as u]))


(defonce ^:private instrs (atom {}))

(defn instr [instr-key] (u/check-nil (@instrs instr-key) "Instr" instr-key))

(defn play-instr
  [instr-key synth params]
  (println "Playing instr" instr-key)
  (swap! instrs assoc instr-key (-> (u/check-nil synth "Synth" instr-key)
                                    (apply params))))

(defn stop-instr
  ([instr-key] (stop-instr instr-key 10))
  ([instr-key num-incrs]
   ; TODO Replace with call to set-param-over-time
   (println "Stopping instr" instr-key "over" num-incrs "increments")
   (let [this-instr (instr instr-key)
         start-amp (ot/node-get-control this-instr :amp)
         amp-delta (/ start-amp num-incrs)
         amps (reverse (take num-incrs (range 0 start-amp amp-delta)))
         start-time (+ (ot/now) 500)
         time-delta 250
         times (take num-incrs (iterate #(+ time-delta %) start-time))
         amps-times (map vector amps times)]
     (doseq [[amp time] amps-times] (ot/at time (ot/ctl this-instr :amp amp)))
     (ot/at (+ time-delta (last times)) (ot/kill this-instr)))))


(defn play-sound
  [instr-key sound-def-key]
  (let [{:keys [synth params]} (snd/sound-def sound-def-key)]
    (play-instr instr-key synth params)))

(defn play-sound-at
  ([time instr-key] (play-sound-at time instr-key instr-key))
  ([time instr-key sound-def-key]
   (ot/apply-by time #(ot/at time (play-sound instr-key sound-def-key)))))

(defn stop-sound
  [time instr-key]
  (ot/apply-by time #'stop-instr [instr-key]))

(defn kill-sound
  [instr-key]
  (ot/kill (instr instr-key)))

(defn- synth-instance
  [type key]
  (case type
    :instr (instr key)
    :trigger (micro/trigger key)
    :pan (micro/pan key)
    (println "Unknown synth type" type)))

(defn set-params
  [type key & params]
  (apply println "Set params for" type key "to" params)
  (apply ot/ctl (synth-instance type key) params))

(defn- set-at [time f & params] (ot/apply-by time #(ot/at time (apply f params))))

(defn set-params-at [time & params] (apply set-at time set-params params))

(defn- get-delta-vals
  [start num-steps f]
  (->> start
       (iterate f)
       (take num-steps)))

(defn set-param-over-time
  [type key param-key num-steps val-delta-f time-delta-f]
  (println "Setting param" param-key "for" type key "over" num-steps "steps")
  (let [synth (synth-instance type key)
        vals (get-delta-vals (ot/node-get-control synth param-key) num-steps val-delta-f)
        times (get-delta-vals (+ (ot/now) 500) num-steps time-delta-f)
        vals-times (map vector vals times)]
    (doseq [[val time] vals-times] (ot/at time (ot/ctl synth param-key val)))))

(defn set-param-over-time-at [time & params] (set-at time set-param-over-time params))
