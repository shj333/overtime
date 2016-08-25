(ns overtime.fx
  (:require [overtone.core :as ot]
            [overtime.utils :as u]
            [overtime.sound-control :as snd]))

(defonce ^:private groups (atom {}))
(defonce ^:private busses (atom {}))


(ot/defsynth reverb1
             [in 0 out 0 room-size 10 rev-time 3 damping 0.5 input-bw 0.5 spread 15 dry-level 1 early-level 0.7 tail-level 0.5 room-size 300]
             (let [sig (ot/in:ar in)
                   dry-level-amp (ot/dbamp dry-level)
                   early-level-amp (ot/dbamp early-level)
                   tail-level-amp (ot/dbamp tail-level)]
               (ot/out:ar out (ot/g-verb:ar sig room-size rev-time damping input-bw spread dry-level-amp early-level-amp tail-level-amp room-size))))


(defn- make-groups
  []
  (let [main-grp (ot/group "main group")
        producer-grp (ot/group "sound gen" :head main-grp)
        fx-grp (ot/group "fx" :after producer-grp)]
    (swap! groups assoc :producer-grp producer-grp :fx-grp fx-grp)
    fx-grp))

(defn- make-synths
  [fx-grp]
  (let [reverb-bus (ot/audio-bus)]
    (swap! busses assoc :reverb-bus reverb-bus)
    (reverb1 [:tail fx-grp] :in reverb-bus :room-size 243 :rev-time 1 :damping 0.1 :input-bw 0.34 :dry-level -3 :early-level -11 :tail-level -9)
    true))

(defn make-fx
  []
  (-> (make-groups)
      make-synths))

(defn group [group-key] (u/check-nil (group-key @groups) "FX Group" group-key))
(defn bus [bus-key] (u/check-nil (bus-key @busses) "FX Bus" bus-key))

(defmethod snd/sound-param :out [_type data] (bus data))
(defmethod snd/sound-grp :fx [_type key] (group key))