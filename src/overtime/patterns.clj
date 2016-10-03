(ns overtime.patterns
  (:require [overtone.core :as ot]
            [overtime.instr-control :as instr]
            [overtime.sound-control :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private patterns (atom {}))
(defonce ^:private dflt-pattern-params {:osc-period 5000 :dur 1000})
(defonce ^:private play-param-key :play-pattern$)


(defn- get-pattern [pattern-key] (u/check-nil (pattern-key @patterns) "Pattern" pattern-key))

(defn- set-params
  [pattern-key & {:as params}]
  (let [pattern (get-pattern pattern-key)]
    ; We can only log the keywords in pattern since values may be a lazy sequence which must not be evaluated!!!
    (log/debug "Changing pattern" pattern-key ", keys:" (keys params))
    (swap! pattern update-in [:params] merge params)
    true))

(defn- get-value
  ([lazy-eval-f val] (get-value lazy-eval-f nil val))
  ([lazy-eval-f key val]
   (cond
     ; If value is a lazy sequence, evaluate with given func
     (seq? val) (lazy-eval-f val)
     ; If a keyword was given, this is a sound parameter, use sound-control logic to get param value
     key (snd/sound-param key val)
     ; Otherwise use the param value as is
     true val)))

(defn- this-synth-params [params] (flatten (for [[k v] params] [k (get-value first k v)])))

(defn- next-synth-params [params] (into {} (for [[k v] params] [k (get-value next k v)])))

(defn- get-pattern-event
  [[prev-event pattern]]
  ; Get next-time from previous event so we know when to play this pattern event
  (let [time (:next-time prev-event)
        {:keys [name synth params]} @pattern
        this-synth (get-value first synth)
        this-params (this-synth-params params)
        this-dur (get-value first (:dur params))
        next-synth (get-value next synth)
        next-params (next-synth-params params)]
    ; Keep playing pattern until one of the params is nil
    (if (not-any? nil? this-params)
      (do
        (log/debug "Pattern:" name ", time:" time ", synth:" this-synth ", params:" this-params)
        ; Swap in next events in pattern
        (swap! pattern assoc :synth next-synth :params next-params)
        ; Pass back this event and pattern so we can iterate over this function and create a lazy sequence
        [{:synth     this-synth
          :params    this-params
          :time      time
          :next-time (+ time this-dur)
          :total-dur (+ (:total-dur prev-event) this-dur)}
         pattern])

      ; Return nil if pattern is complete indicating that pattern should stop playing
      (do
        (log/debug "Pattern complete:" name)
        nil))))

(defn- is-not-stage-complete?
  [osc-period event]
  ; Stage is complete if next event is nil OR total duration of events in this OSC cmd cycle has gone beyond OSC period
  (-> (or (nil? event) (>= (:total-dur event) osc-period))
      not))

(defn- get-pattern-events
  [time pattern]
  ; Get pattern events that go in current OSC command cycle (length of cycle defined by pattern's osc-period) and play them at the
  ; given time. We keep the time of the next event in pattern in :next-time value and the total duration of events in this OSC command
  ; cycle in :total-dur value within pattern.
  (let [osc-period (get-in @pattern [:params :osc-period])
        ; For first event in the OSC command cycle, call get-pattern-event function using a previous event that declares the "next time" to be at given time
        [first-event] (get-pattern-event [{:next-time time :total-dur 0} pattern])
        ; Set up lazy sequence of events remaining in pattern
        lazy-events (iterate get-pattern-event [first-event pattern])
        ; Split up lazy list into events in this OSC command cycle and those in the future
        [head tail] (split-with (partial is-not-stage-complete? osc-period) (map first lazy-events))]
    ; We need to take the first event of tail since it's already been consumed within lazy sequence in management above
    (concat head (take 1 tail))))

(defn- convert-for-s_new
  [param]
  ; We use "/s_new" OSC command instead of synth function for each pattern event (more efficient), so we have to convert the event's
  ; params in same way as synth function does in Overtone (only allowed values are floats and strings).
  (let [param (ot/to-id param)]
    (cond
      (keyword? param) (name param)
      (number? param) (float param)
      (true? param) (float 1)
      (false? param) (float 0)
      true param)))

(defn- do-pattern-event
  [time synth params]
  ; Set up a single pattern event to be played on SC server. Use "/s_new" OSC command instead of calling synth function to cut down
  ; on overhead. This efficiency helps when dealing with microsound-based patterns.
  (if-let [synth-name (get-in synth [:sdef :name])]
    (ot/at time (apply ot/snd "/s_new" synth-name -1 0 0 (map convert-for-s_new params)))))

(defn- do-pattern-events
  [events]
  (doseq [{:keys [synth params time]} events] (do-pattern-event time synth params))
  events)

(defn- setup-next-pattern-events
  [f pattern-key {:keys [next-time]}]
  ; If pattern has not stopped playing, then set up next OSC command cycle to be run on a new thread when next event in pattern starts
  (when-not (nil? next-time) (u/apply-by next-time (f next-time pattern-key))))

(defn- play-pattern
  [time pattern-key]
  ; Play events in this OSC command cycle and set up next OSC command cycle
  (->> (get-pattern pattern-key)
       (get-pattern-events time)
       do-pattern-events
       last
       (setup-next-pattern-events play-pattern pattern-key)))



;
; Public API
;

(defn make-pattern
  "Create a new pattern identified by the given key. Uses the given Overtone synth to produce sounds by reading the params map during
  each cycle. Each value in the params map can be a lazy sequence or a constant value. If the value (either in a lazy sequence or constant)
  is a keyword, then the overtime.sound-control/sound-param multi-method is used to obtain the value for the current cycle; this is so
  that busses, effects, etc can be set in each pattern event.

  Pattern events are pooled together until the osc-period period has elapsed and then sent together in one OSC command to SC server in
  order to cut down on the amount of communications with the server. This is especially useful with microsound-based patterns.

  The paramters depend on the synth definition. However, two special parameters are used when playing the cycle:

  :dur - Duration of each event in the pattern in milliseconds. Default: 1000ms
  :osc-period - Duration of of each OSC cycle in milliseconds (i.e., the amount of time between each OSC command sent to SC server). Default: 5000ms

  Example params map:
  {:out-bus    :reverb1
   :freq       (cycle [440 220])
   :sustain    0.1
   :pan        (cycle [-1 0 1])
   :amp        0.1
   :dur        25}"
  [pattern-key synth params]
  ; Each pattern is an atom so we can manage the workflow of the pattern across threads. All pattern atoms are stored in patterns atom so we can manage
  ; all the patterns created by this function.
  (->> (atom {:name (name pattern-key) :synth synth :params (merge dflt-pattern-params params)})
       (swap! patterns assoc pattern-key))
  (log/debug "Created pattern" pattern-key)
  true)

(defn play-at
  "Start playing pattern at the given time. Time is defined in terms of Overtone now function."
  [time pattern-key]
  ; Reset play-param in case it was set to nil to stop the pattern previously (see stop function)
  (set-params pattern-key play-param-key true)
  (u/apply-by time (do
                     (log/info "Starting pattern" pattern-key)
                     (play-pattern time pattern-key))))

(defn stop-at
  "Stop playing pattern at the given time. Time is defined in terms of Overtone now function."
  [time pattern-key]
  ; By setting one of the pattern params to nil, the pattern stops on next read in cycle (see get-pattern-event func)
  ; We use a special pattern param as not to conflict with any params in pattern set by user
  (u/apply-by time (do
                     (log/info "Stopping pattern" pattern-key)
                     (set-params pattern-key play-param-key nil))))

(defn current-value
  "Returns the current value of the given parameter within the pattern"
  [pattern-key param-key]
  (-> (get-pattern pattern-key)
      deref
      (get-in [:params param-key])))



;
; Multi method definitions wrt patterns
;
(defmethod instr/set-params :pat [_type pattern-key & params] (apply set-params pattern-key params))
(defmethod instr/play-sound-at :pat [time _instr-type & [pattern-key]] (play-at time pattern-key))
(defmethod instr/stop-sound-at :pat [time _instr-type pattern-key] (stop-at time pattern-key))



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
  (play-at (+ (ot/now) 1000) :gabor1)
  (set-params :gabor1 :dur 25)
  (set-params :gabor1 :dur 1000)
  (stop-at (+ (ot/now) 1000) :gabor1))