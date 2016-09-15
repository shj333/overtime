(ns overtime.bus-control
  (:require [overtone.core :as ot]
            [overtime.sound-control :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private busses (atom {}))


(defn- init-busses
  [bus-data]
  (into {} (for [[bus-key num-chans] bus-data] [bus-key (ot/control-bus num-chans)])))

(defn init
  [bus-data]
  (let [new-busses (init-busses bus-data)]
    (reset! busses new-busses)
    (log/debug "Created" (count new-busses) "control busses for keys" (keys new-busses))
    true))

(defn control-bus [key] (u/check-nil (key @busses) "Control Bus" key))
(defn control-bus-keys [] (keys @busses))
(defmethod snd/sound-param-keyword-f :control-bus [_type] control-bus)
