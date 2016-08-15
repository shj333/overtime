(ns overtime.instr-control
  (:require [overtone.core :as ot]
            [overtime.microsound :as micro]
            [overtime.sound-control :as snd]))


(defonce ^:private instrs (atom {}))

(defn play-instr
  [instr-key synth params]
  (println "Playing instr" instr-key)
  (swap! instrs assoc instr-key (apply synth params)))

(defn stop-instr
  ([instr-key] (stop-instr instr-key 10))
  ([instr-key num-incrs]
   (println "Stopping instr" instr-key "over" num-incrs "increments")
   (let [this-instr (@instrs instr-key)
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
  (ot/kill (instr-key @instrs)))

(defn set-params
  [type key & params]
  (let [synth-instance (case type
                         :instr (@instrs key)
                         :trigger (micro/trigger key)
                         :pan (micro/pan key)
                         (println "Unknown synth type" type))]
    (apply ot/ctl synth-instance params)))

(defn set-params-at
  [time type key & params]
  (ot/apply-by time #(ot/at time
                            (apply println "Set params for" type key "to" params)
                            (apply set-params type key params))))
