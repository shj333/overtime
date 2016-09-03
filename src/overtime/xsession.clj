(ns overtime.xsession
  (:require [overtone.core :as ot]))


;(ot/midi-connected-devices)
;(ot/event-debug-on)
;(ot/event-debug-off)

(def xsession-map {24 :1-left
                   25 :2-left
                   26 :3-left
                   27 :high-left
                   28 :mid-left
                   29 :low-left
                   12 :pitch-left
                   11 :volume-left

                   44 :headphone-left
                   46 :rewind-left
                   43 :ff-left
                   58 :cue-left
                   70 :play-left

                   31 :1-right
                   32 :2-right
                   33 :3-right
                   34 :high-right
                   35 :mid-right
                   36 :low-right
                   15 :pitch-right
                   14 :volume-right

                   45 :headphone-right
                   56 :rewind-right
                   57 :ff-right
                   59 :cue-right
                   69 :play-right

                   17 :fader
                   20 :inv-fader})

(defmulti on-event (fn [{note :note}] (get xsession-map note :unmapped)))
(defmethod on-event :default [{note :note velocity :velocity}] (println "Unknown MIDI event:" note ", velocity:" velocity))
(defmethod on-event :1-left [{velocity :velocity}] (println "1 left:" velocity))
(defmethod on-event :2-left [{velocity :velocity}] (println "2 left:" velocity))
(defmethod on-event :3-left [{velocity :velocity}] (println "3 left:" velocity))
(defmethod on-event :high-left [{velocity :velocity}] (println "High left:" velocity))
(defmethod on-event :mid-left [{velocity :velocity}] (println "Mid left:" velocity))
(defmethod on-event :low-left [{velocity :velocity}] (println "Low left:" velocity))
(defmethod on-event :pitch-left [{velocity :velocity}] (println "Pitch left:" velocity))
(defmethod on-event :volume-left [{velocity :velocity}] (println "Volume left:" velocity))

(defmethod on-event :1-right [{velocity :velocity}] (println "1 right:" velocity))
(defmethod on-event :2-right [{velocity :velocity}] (println "2 right:" velocity))
(defmethod on-event :3-right [{velocity :velocity}] (println "3 right:" velocity))
(defmethod on-event :high-right [{velocity :velocity}] (println "High right:" velocity))
(defmethod on-event :mid-right [{velocity :velocity}] (println "Mid right:" velocity))
(defmethod on-event :low-right [{velocity :velocity}] (println "Low right:" velocity))
(defmethod on-event :pitch-right [{velocity :velocity}] (println "Pitch right:" velocity))
(defmethod on-event :volume-right [{velocity :velocity}] (println "Volume right:" velocity))

(defmethod on-event :headphone-left [_event] (println "Headphone left"))
(defmethod on-event :rewind-left [_event] (println "Rewind left"))
(defmethod on-event :ff-left [_event] (println "FF left"))
(defmethod on-event :cue-left [_event] (println "Cue left"))
(defmethod on-event :play-left [_event] (println "Play left"))

(defmethod on-event :fader [{velocity :velocity}] (println "Fade:" velocity))
(defmethod on-event :inv-fader [{velocity :velocity}] (println "Inverted Fade:" velocity))

(defmethod on-event :headphone-right [_event] (println "Headphone right"))
(defmethod on-event :rewind-right [_event] (println "Rewind right"))
(defmethod on-event :ff-right [_event] (println "FF right"))
(defmethod on-event :cue-right [_event] (println "Cue right"))
(defmethod on-event :play-right [_event] (println "Play right"))

(defn- set-event-hdlr
  [type hdlr-key]
  (ot/on-event
    [:midi type]
    (fn [event] (on-event event))
    hdlr-key)
  )
(defn start [] (map set-event-hdlr [:control-change :note-on] [::cc-hdlr ::note-hdlr]))
(defn stop [] (map ot/remove-event-handler [::cc-hdlr ::note-hdlr]))

(comment
  (defmethod on-event :1-left [{velocity :velocity}] (println "Density:" velocity))
  (defmethod on-event :high-left [{velocity :velocity}] (println "Grain Dur:" velocity))
  (defmethod on-event :2-left [{velocity :velocity}] (println "Freq:" velocity))
  (defmethod on-event :mid-left [{velocity :velocity}] (println "Freq Dev Noise:" velocity))
  (defmethod on-event :3-left [{velocity :velocity}] (println "Mod Freq:" velocity))
  (defmethod on-event :volume-left [{velocity :velocity}] (println "Amp:" velocity))
  )
