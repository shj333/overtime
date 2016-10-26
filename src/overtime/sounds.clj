(ns overtime.sounds
  (:require [overtime.groups :as grp]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))

(defonce ^:private sound-defs (atom {}))
(defonce ^:private patterns (atom {}))


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

(defn get-pattern [pattern-key] (u/check-nil (pattern-key @patterns) "Pattern" pattern-key))
(defn pattern-keys [] (keys @patterns))
(defn pattern? [key] (contains? @patterns key))
(defn add-pattern [key pattern] (swap! patterns assoc key pattern))



(defmulti sound-control
          (fn [_time [sound-control key & _rest-event-data]]
            (if (contains? #{:play :stop} sound-control)
              (cond
                (sound-def? key) (keyword (str (name sound-control) "-instr"))
                (pattern? key) (keyword (str (name sound-control) "-pat"))
                true (log/error "Unknown key" key "for sound control" sound-control))
              sound-control)))

(defmethod sound-control :default [_time event-data] (log/error "Unknown sound control" event-data))
