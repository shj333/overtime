(ns overtime.sound-control
  (:require [overtime.bus-control :as bus]
            [overtime.groups-control :as grp]
            [overtime.microsound :as micro]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private sound-defs (atom {}))

(defmulti sound-param-val (fn [type _val] type))
(defmethod sound-param-val :default [_type val] val)
(defmethod sound-param-val :control-bus [_type val] (bus/bus :control val))
(defmethod sound-param-val :in [_type val] (bus/bus :fx val))
(defmethod sound-param-val :out [_type val] (bus/bus :fx val))
(defmethod sound-param-val :env-buf [_type val] (micro/env-buf val))
(defmethod sound-param-val :trigger-bus [_type val] (bus/bus :trigger val))
(defmethod sound-param-val :pan-bus [_type val] (bus/bus :pan val))

(defn sound-param
  [key val]
  (if (keyword? val)
    (sound-param-val key val)
    val))

(defn- define-sound
  [key {:keys [synth group group-pos params] :or {group :producers group-pos :tail}}]
  (let [params-list (->> (flatten (for [[key data] params] [key (sound-param key data)]))
                         (cons [group-pos (grp/group group)]))
        sound-def {:synth synth :params params-list}]
    (log/debug "Defined sound for" key "in group" group)
    [key sound-def]))


(defn define-sounds
  [{:keys [busses micro def-synths-f sounds]}]
  (grp/init)
  (bus/init busses)
  (micro/init micro)
  (when def-synths-f
    (def-synths-f)
    (log/debug "Finished defining synths"))
  (->> (into {} (for [[key sound] sounds] (define-sound key sound)))
       (swap! sound-defs merge))
  (log/debug "Finished defining sounds")
  true)

(defn sound-def [key] (u/check-nil (key @sound-defs) "Sound Def" key))