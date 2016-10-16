(ns overtime.midi-controller-gui
  (:require [overtone.core :as ot]
            [overtime.instruments :as instr]
            [overtime.instr-gui :as gui]
            [overtime.midi-controllers :as mc]
            [overtime.utils :as u]))


(defn- init-xs-hdlrs
  [frame-key & {:keys [device instr-type instr sliders] :or {sliders []}}]
  (mc/add-handler! device :play-left frame-key (fn [_x] (instr/play-sound-at (+ (ot/now) 500) instr-type instr)))
  (mc/add-handler! device :cue-left frame-key (fn [_x] (instr/stop-sound-at (+ (ot/now) 500) instr-type instr)))
  (doseq [[slider-key {:keys [knob-key min max slider-f] :or {slider-f u/num-lin-lin}}] sliders]
    (mc/add-handler! device knob-key frame-key #(gui/change-slider-val frame-key slider-key (slider-f % 0 127 min max)))))

(defn show
  [frame-key & params]
  (apply init-xs-hdlrs frame-key params)
  (apply gui/show frame-key params))
