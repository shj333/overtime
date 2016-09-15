(ns overtime.patterns
  (:require [overtone.core :as ot]
            [overtime.sect-control :as sect]
            [overtime.sound-control :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private patterns (atom {}))
(defonce ^:private dflt-pattern-params {:osc-period 5000})

(defn- get-pattern [pattern-key] (u/check-nil (pattern-key @patterns) "Pattern" pattern-key))

(defn- get-value
  ([lazy-eval-f val] (get-value lazy-eval-f nil val))
  ([lazy-eval-f key val]
   (cond
     ; If value is a lazy sequence, evaluate with given func
     (seq? val) (lazy-eval-f val)
     ; If a key was given, this is a sound parameter, use sound-control logic to get param value
     key (snd/sound-param key val)
     ; Otherwise use the param value as is
     true val)))

(defn- this-synth-params
  [params]
  (flatten (for [[k v] params] [k (get-value first k v)])))

(defn- next-synth-params
  [params]
  (into {} (for [[k v] params] [k (get-value next k v)])))

(defn- get-pattern-event
  [[prev-event pattern]]
  (let [time (:next-time prev-event)
        {:keys [name synth params]} @pattern
        this-synth (get-value first synth)
        this-params (this-synth-params params)
        this-dur (get-value first (:dur params))
        next-synth (get-value next synth)
        next-params (next-synth-params params)]
    (if (not-any? nil? this-params)
      (do
        (log/debug "Pattern:" name ", time:" time ", synth:" this-synth ", params:" this-params)
        (swap! pattern assoc :synth next-synth :params next-params)
        [{:synth     this-synth
          :params    this-params
          :time      time
          :next-time (+ time this-dur)
          :total-dur (+ (:total-dur prev-event) this-dur)}
         pattern])
      (do
        (log/debug "Pattern complete:" name)
        nil))))

(defn- is-not-stage-complete?
  [osc-period event]
  (if (nil? event)
    false
    (< (:total-dur event) osc-period)))

(defn- get-pattern-events
  [time pattern]
  (let [osc-period (get-in @pattern [:params :osc-period])
        [first-event] (get-pattern-event [{:next-time time :total-dur 0} pattern])
        lazy-events (->> (iterate get-pattern-event [first-event pattern])
                         (map first))
        [head tail] (split-with (partial is-not-stage-complete? osc-period) lazy-events)]
    (concat head (take 1 tail))))

(defn- convert-for-s_new
  [param]
  (let [param (ot/to-id param)]
    (cond
      (keyword? param) (name param)
      (number? param) (float param)
      (true? param) (float 1)
      (false? param) (float 0)
      true param)))

(defn- do-pattern-event
  [time synth params]
  (if-let [synth-name (get-in synth [:sdef :name])]
    (ot/at time (apply ot/snd "/s_new" synth-name -1 0 0 (map convert-for-s_new params)))))

(defn- do-pattern-events
  [events]
  (doseq [{:keys [synth params time]} events] (do-pattern-event time synth params))
  events)

(defn- setup-next-pattern-events
  [f pattern-key last-event]
  (if-not (nil? last-event)
    (let [{:keys [next-time]} last-event]
      (u/apply-by next-time (f next-time pattern-key)))))

(defn do-pattern
  [time pattern-key]
  (->> (get-pattern pattern-key)
       (get-pattern-events time)
       do-pattern-events
       last
       (setup-next-pattern-events do-pattern pattern-key)))

(defn do-pattern-at
  [time pattern-key]
  (log/info "Starting pattern" pattern-key)
  (u/apply-by time (do-pattern time pattern-key)))

(defn make-pattern
  [pattern-key synth params]
  (->> (atom {:name (name pattern-key) :synth synth :params (merge dflt-pattern-params params)})
       (swap! patterns assoc pattern-key))
  true)

(defn change-pattern
  [pattern-key & params]
  (let [pattern (get-pattern pattern-key)
        mapped-params (apply hash-map params)]
    (log/info "Changing pattern" pattern-key ", keys:" (keys mapped-params))
    (swap! pattern update-in [:params] merge mapped-params)
    true))

(defn change-pattern-at [time pattern-key & params] (u/apply-by time (apply change-pattern pattern-key params)))

(defn pattern-value
  [pattern-key param-key]
  (-> (get-pattern pattern-key)
      deref
      (get-in [:params param-key])))


(defmethod sect/instr-control-f :pat [_event-data] do-pattern-at)
(defmethod sect/instr-control-f :patd [_event-data] change-pattern-at)


(comment
  (if (ot/server-disconnected?) (ot/connect-external-server 4445))

  (ot/defsynth gabor [out-bus [0 :ir] freq [440 :ir] sustain [1 :ir] pan [0.0 :ir] amp [0.1 :ir] width [0.25 :ir]]
               (let [env (ot/lf-gauss:ar sustain width :loop 0 :action ot/FREE)
                     half-pi (* 0.5 (. Math PI))
                     son (* env (ot/f-sin-osc:ar freq half-pi))]
                 (ot/offset-out:ar out-bus (ot/pan2:ar son pan amp))))

  (make-pattern :gabor1
                gabor
                {:out-bus    0
                 :freq       (cycle [440 220])
                 :sustain    0.1
                 :pan        0.0
                 :amp        0.1
                 :width      0.25
                 :dur        1000
                 :osc-period 2000})
  (do-pattern-at (+ (ot/now) 1000) :gabor1)
  (change-pattern :gabor1 :dur 25)
  (change-pattern :gabor1 :amp nil))