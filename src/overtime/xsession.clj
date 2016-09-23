(ns overtime.xsession
  (:require [overtone.core :as ot]))


;(ot/midi-connected-devices)
;(ot/event-debug-on)
;(ot/event-debug-off)

(defonce ^:private handlers-per-knob (atom {}))


(defonce ^:private xsession-map {24 :1-left
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

(defn- get-midi-note
  [event]
  (let [midi-note (-> (:note event)
                      xsession-map)]
    (if (nil? midi-note)
      (throw (str "Unknown XSession MIDI note:" midi-note))
      midi-note)))

(defn- set-midi-event-hdlr
  [event-type hdlr-key]
  (ot/on-event
    [:midi event-type]
    (fn [event]
      (let [handlers ((get-midi-note event) @handlers-per-knob)]
        (doseq [[_k f] handlers] (f (:velocity event)))))
    hdlr-key))

(defn start [] (doseq [[event-type hdlr-key] [[:control-change ::cc-hdlr] [:note-on ::note-hdlr]]] (set-midi-event-hdlr event-type hdlr-key)))
(defn stop [] (doseq [hdlr-key [::cc-hdlr ::note-hdlr]] (ot/remove-event-handler hdlr-key)))

(defn add-handler!
  [knob key f]
  (swap! handlers-per-knob assoc-in [knob key] f))

(defn remove-handler!
  [knob key]
  (swap! handlers-per-knob update knob dissoc key))

(defn clear-handlers!
  [key]
  (doseq [knob (keys @handlers-per-knob)] (remove-handler! knob key)))

(defn dump-handlers [] (doseq [[knob-key val] @handlers-per-knob] (println knob-key "=>" (keys val))))

(comment
  (start)
  (stop)
  (add-handler! :1-left :instr1 #(println "1-left:" %))
  (add-handler! :1-left :instr1 #(println "1-left other:" %))
  (add-handler! :1-left :instr2 #(println "1-left instr2:" %))
  (remove-handler! :1-left :instr2)
  (add-handler! :high-left :instr1 #(println "high-left:" %))
  (add-handler! :2-left :instr1 #(println "2-left:" %))
  (add-handler! :mid-left :instr1 #(println "mid-left:" %))
  (add-handler! :3-left :instr1 #(println "3-left:" %))
  (add-handler! :volume-left :instr1 #(println "volume-left:" %))
  (add-handler! :headphone-left :instr1 (fn [_x] (println "headphone-left")))
  (clear-handlers! :instr1)
  (dump-handlers))
