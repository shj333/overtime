(ns overtime.sound-control
  (:require [overtone.core :as ot]
            [overtime.microsound :as micro]
            [overtime.fx :as fx]))


(defonce ^:private sound-defs (atom {}))
(defn- param-data
  [key data]
  (case key
    :env-buf (micro/env-buf data)
    :trigger-bus (micro/trigger-bus data)
    :pan-bus (micro/pan-bus data)
    :out (fx/bus data)
    data))

(defn- define-sound
  [sound-def-key sound-def-data]
  (let [{:keys [synth params]} sound-def-data
        params-list (->> (flatten (for [[key data] params] [key (param-data key data)]))
                         (cons [:tail (fx/group :producer-grp)]))
        sound-def {:synth synth :params params-list}]
    [sound-def-key sound-def]))

(defn define-sounds
  [sound-defs-data]
  (swap! sound-defs merge (into {} (for [[sound-def-key sound-def-data] sound-defs-data] (define-sound sound-def-key sound-def-data)))))
