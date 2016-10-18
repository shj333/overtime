(ns overtime.midi-controller-gui
  (:require [overtone.core :as ot]
            [overtime.instruments :as instr]
            [overtime.instr-gui :as gui]
            [overtime.midi-controllers :as mc]))

(defn- dflt-slider-f [val-f min max] (* val-f (- max min)))

(defn- init-xs-hdlrs
  [frame-key & {:keys [instr-type instr device sliders play stop] :or {sliders []}}]
  (mc/add-handler! device play frame-key (fn [_val _val-f] (instr/play-sound-at (+ (ot/now) 500) instr-type instr)))
  (mc/add-handler! device stop frame-key (fn [_val _val-f] (instr/stop-sound-at (+ (ot/now) 500) instr-type instr)))
  (doseq [[slider-key {:keys [knob-key min max slider-f] :or {slider-f dflt-slider-f}}] sliders]
    (mc/add-handler! device knob-key frame-key (fn [_val val-f] (gui/change-slider-val frame-key slider-key (slider-f val-f min max))))))

(defn show
  [frame-key & params]
  (apply init-xs-hdlrs frame-key params)
  (apply gui/show frame-key params))
