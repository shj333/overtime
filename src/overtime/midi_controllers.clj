(ns overtime.midi-controllers
  (:require [overtone.core :as ot]
            [clojure.tools.logging :as log]))


(defonce ^:private handlers-per-knob (atom {}))


(defonce ^:private device-map {"USB X-Session"  :x-session
                               "Launch Control" :launch})

(defonce ^:private knob-map {:x-session {24 :1-left
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
                                         20 :inv-fader}

                             :launch    {13  :1-send-a
                                         14  :2-send-a
                                         15  :3-send-a
                                         16  :4-send-a
                                         17  :5-send-a
                                         18  :6-send-a
                                         19  :7-send-a
                                         20  :8-send-a

                                         29  :1-send-b
                                         30  :2-send-b
                                         31  :3-send-b
                                         32  :4-send-b
                                         33  :5-send-b
                                         34  :6-send-b
                                         35  :7-send-b
                                         36  :8-send-b

                                         49  :1-pan
                                         50  :2-pan
                                         51  :3-pan
                                         52  :4-pan
                                         53  :5-pan
                                         54  :6-pan
                                         55  :7-pan
                                         56  :8-pan

                                         77  :1-fader
                                         78  :2-fader
                                         79  :3-fader
                                         80  :4-fader
                                         81  :5-fader
                                         82  :6-fader
                                         83  :7-fader
                                         84  :8-fader

                                         41  :1-focus
                                         42  :2-focus
                                         43  :3-focus
                                         44  :4-focus
                                         57  :5-focus
                                         58  :6-focus
                                         59  :7-focus
                                         60  :8-focus

                                         73  :1-control
                                         74  :2-control
                                         75  :3-control
                                         76  :4-control
                                         89  :5-control
                                         90  :6-control
                                         91  :7-control
                                         92  :8-control

                                         104 :up-btn
                                         105 :down-btn
                                         106 :left-btn
                                         107 :right-btn

                                         108 :device
                                         109 :mute
                                         110 :solo
                                         111 :record}})

(defn- get-device-name [event] (clojure.string/trim (subs (nth (:dev-key event) 3) 0 14)))

(defn- get-device-key
  [event]
  (let [device-name (get-device-name event)
        device-key (device-map device-name)]
    (if (nil? device-key)
      (log/error "Unknown MIDI device:" device-name)
      device-key)))

(defn- get-midi-knob
  [device-key event]
  (let [knob (get-in knob-map [device-key (:note event)])]
    (if (nil? knob)
      (log/error "Unknown note for device" (name device-key) ":" (:note event))
      knob)))

(defn- set-midi-event-hdlr
  [event-type hdlr-key]
  (ot/on-event
    [:midi event-type]
    (fn [event]
      (let [device-key (get-device-key event)
            handlers (-> (device-key @handlers-per-knob)
                         (get (get-midi-knob device-key event)))]
        (doseq [[_k f] handlers] (f (:velocity event)))))
    hdlr-key))

(defn start [] (doseq [[event-type hdlr-key] [[:control-change ::cc-hdlr] [:note-on ::note-hdlr]]] (set-midi-event-hdlr event-type hdlr-key)))
(defn stop [] (doseq [hdlr-key [::cc-hdlr ::note-hdlr]] (ot/remove-event-handler hdlr-key)))

(defn add-handler!
  [device knob key f]
  (swap! handlers-per-knob assoc-in [device knob key] f))

(defn remove-handler!
  [device knob key]
  (swap! handlers-per-knob update-in [device knob] dissoc key))

(defn clear-handlers!
  [device key]
  (doseq [knob (keys (device @handlers-per-knob))] (remove-handler! device knob key)))

(defn dump-handlers
  []
  (doseq [[device-key knobs-map] @handlers-per-knob]
    (println (name device-key))
    (doseq [[knob-key val] knobs-map] (if (some? (keys val)) (println "\t" knob-key "=>" (keys val))))))


(comment
  (ot/midi-connected-devices)
  (ot/event-debug-on)
  (ot/event-debug-off)
  (ot/on-event
    [:midi :control-change]
    (fn [event] (do
                  ;(println event)
                  (println (str (get-device-name event) " [" (device-map (get-device-name event)) "]: " (get-midi-knob (get-device-key event) event)))))
    ::test-midi)
  (ot/remove-event-handler ::test-midi)

  (start)
  (stop)
  (add-handler! :x-session :1-left :instr1 #(println "1-left:" %))
  (add-handler! :x-session :1-left :instr1 #(println "1-left other:" %))
  (add-handler! :x-session :1-left :instr2 #(println "1-left instr2:" %))
  (remove-handler! :x-session :1-left :instr2)
  (add-handler! :x-session :high-left :instr1 #(println "high-left:" %))
  (add-handler! :x-session :2-left :instr1 #(println "2-left:" %))
  (add-handler! :x-session :mid-left :instr1 #(println "mid-left:" %))
  (add-handler! :x-session :3-left :instr1 #(println "3-left:" %))
  (add-handler! :x-session :volume-left :instr1 #(println "volume-left:" %))
  (add-handler! :x-session :headphone-left :instr1 (fn [_x] (println "headphone-left")))
  (clear-handlers! :x-session :instr1)

  (add-handler! :launch :1-send-a :instr1 #(println "1-send-a:" %))
  (add-handler! :launch :1-send-a :instr1 #(println "1-send-a other:" %))
  (add-handler! :launch :1-send-a :instr2 #(println "1-send-a instr2:" %))
  (remove-handler! :launch :1-send-a :instr2)
  (add-handler! :launch :2-send-a :instr1 #(println "2-send-a:" %))
  (add-handler! :launch :1-send-b :instr1 #(println "1-send-b:" %))
  (add-handler! :launch :2-send-b :instr1 #(println "2-send-b:" %))
  (add-handler! :launch :1-fader :instr1 #(println "1-fader:" %))
  (add-handler! :launch :1-focus :instr1 (fn [_x] (println "1-focus")))
  (add-handler! :launch :1-control :instr1 (fn [_x] (println "1-control")))
  (clear-handlers! :launch :instr1)

  (dump-handlers))
