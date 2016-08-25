(ns overtime.sound-control
  (:require [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private sound-defs (atom {}))

(defmulti sound-param (fn [type _data] type))
(defmethod sound-param :default [_type data] data)

(defmulti sound-grp (fn [type _key] type))
(defmethod sound-grp :default [type _data] (log/error "Unknown sound group type" type))


(defn- define-sound
  [sound-def-key sound-def-data]
  (let [{:keys [synth params]} sound-def-data
        params-list (->> (flatten (for [[key data] params] [key (sound-param key data)]))
                         (cons [:tail (sound-grp :fx :producer-grp)]))
        sound-def {:synth synth :params params-list}]
    [sound-def-key sound-def]))

(defn define-sounds
  [sound-defs-data]
  (swap! sound-defs merge (into {} (for [[sound-def-key sound-def-data] sound-defs-data] (define-sound sound-def-key sound-def-data)))))

(defn sound-def [key] (u/check-nil (key @sound-defs) "Sound Def" key))