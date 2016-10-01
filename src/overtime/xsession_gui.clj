(ns overtime.xsession-gui
  (:require [overtone.core :as ot]
            [overtime.instr-gui :as gui]
            [overtime.patterns :as pat]
            [overtime.xsession :as xs]
            [overtime.utils :as u]))


(defn- init-xs-hdlrs
  [frame-key & {:keys [instr-type instr sliders] :or {sliders []}}]
  (when (= instr-type :pat)
    (xs/add-handler! :play-left frame-key (fn [_x] (pat/play-at (+ (ot/now) 1000) instr)))
    (xs/add-handler! :cue-left frame-key (fn [_x] (pat/stop instr)))
    (doseq [[slider-key {:keys [knob-key min max slider-f] :or {slider-f u/num-lin-lin}}] sliders]
      (xs/add-handler! knob-key frame-key #(gui/change-slider-val frame-key slider-key (slider-f % 0 127 min max))))))

(defn show
  [frame-key & params]
  (apply init-xs-hdlrs frame-key params)
  (apply gui/show frame-key params))
