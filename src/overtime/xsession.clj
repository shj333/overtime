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
  [type hdlr-key]
  (ot/on-event
    [:midi type]
    (fn [event]
      (let [handlers ((get-midi-note event) @handlers-per-knob)]
        (doseq [[_k f] handlers] (f (:velocity event)))))
    hdlr-key))

(defn start [] (map set-midi-event-hdlr [:control-change :note-on] [::cc-hdlr ::note-hdlr]))
(defn stop [] (map ot/remove-event-handler [::cc-hdlr ::note-hdlr]))

(defn add-handler!
  [xs-event key f]
  (swap! handlers-per-knob update-in [xs-event] assoc key f))

(defn remove-handler!
  [xs-event key]
  (swap! handlers-per-knob update-in [xs-event] dissoc key))

(defn dump-handlers [] (doseq [[knob-key val] @handlers-per-knob] (println knob-key "=>" (keys val))))

(comment
  (start)
  (stop)
  (add-handler! :1-left :foo #(println "1-left:" %))
  (add-handler! :1-left :foo #(println "1-left other:" %))
  (add-handler! :1-left :bar #(println "1-left bar:" %))
  (remove-handler! :1-left :bar)
  (add-handler! :high-left :foo #(println "high-left:" %))
  (add-handler! :2-left :foo #(println "2-left:" %))
  (add-handler! :mid-left :foo #(println "mid-left:" %))
  (add-handler! :3-left :foo #(println "3-left:" %))
  (add-handler! :volume-left :foo #(println "volume-left:" %))
  (add-handler! :headphone-left :foo (fn [_x] (println "headphone-left")))
  (dump-handlers))
