(ns overtime.sounds
  (:require [overtime.groups :as grp]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))

(defonce ^:private sound-defs (atom {}))


(defmulti sound-param-val (fn [type _val] type))
(defmethod sound-param-val :default [_type val] val)

(defn sound-param
  [key val]
  (if (keyword? val)
    (sound-param-val key val)
    val))


(defn- make-sound-def
  [key {:keys [synth group group-pos params] :or {group :producers group-pos :tail}}]
  (let [params-list (->> (flatten (for [[key data] params] [key (sound-param key data)]))
                         (cons [group-pos (grp/group group)]))
        sound-def {:synth synth :params params-list}]
    (log/debug "Defined sound for synth" synth "in group" group "using key" key)
    [key sound-def]))

(defn init
  [sounds]
  (->> (into {} (for [[key sound] sounds] (make-sound-def key sound)))
       (swap! sound-defs merge))
  (log/info "Finished sounds init:" (keys @sound-defs))
  true)

(defn sound-def [key] (u/check-nil (key @sound-defs) "Sound Def" key))
(defn sound-def? [key] (contains? @sound-defs key))
