(ns overtime.patterns
  (:require [overtone.core :as ot]
            [overtime.sounds :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private patterns (atom {}))
(defonce ^:private dflt-pattern-params {:dur 1000 :staging-period 5000})
(defonce ^:private play-param-key :play-pattern$)


(defn- get-pattern [pattern-key] (u/check-nil (pattern-key @patterns) "Pattern" pattern-key))

(defn- pattern? [key] (contains? @patterns key))

(defn- pattern-keys [] (keys @patterns))

(defn- print-params
  [params]
  ; Values may be a lazy sequence which must not be evaluated
  (->> (for [[k v] params] (str k " => " (if (seq? v) "seq" v)))
       (clojure.string/join ", ")))

(defn- enqueue-param-changes
  [pattern-key & {:as params}]
  (let [pattern (get-pattern pattern-key)]
    (log/debug "Enqueueing pattern changes for" (:name @pattern) "to:" (print-params params))
    (swap! pattern update-in [:new-params] merge params)
    true))

(defn- dequeue-param-changes
  ; Pattern is de-referenced since this function is called by swap!
  [derefed-pattern]
  (if (empty? (:new-params derefed-pattern))
    derefed-pattern
    (do
      (log/debug "Changing pattern" (:name derefed-pattern) "to:" (print-params (:new-params derefed-pattern)))
      ; Merge in queued param changes and then reset queue to empty
      (-> derefed-pattern
          (update-in [:params] merge (:new-params derefed-pattern))
          (assoc :new-params {})))))

(defn- reset-pattern
  ; Pattern is de-referenced since this function is called by swap!
  [derefed-pattern]
  (log/debug "Resetting pattern" (:name derefed-pattern) "to original synth and params")
  (assoc derefed-pattern :synth (:orig-synth derefed-pattern) :params (:orig-params derefed-pattern) :reset-flag false))

(defn- get-value
  ([lazy-eval-f val] (get-value lazy-eval-f nil val))
  ([lazy-eval-f key val]
   (cond
     ; If value is a lazy sequence, evaluate with given func
     (seq? val) (lazy-eval-f val)
     ; If a keyword was given, this is a sound parameter
     key (snd/sound-param key val)
     ; Otherwise use the param value as is
     true val)))

(defn- this-synth
  [synth]
  ; Dereference synth value if synth is defined as a var
  (let [this-synth-val (get-value first synth)]
    (if (var? this-synth-val) @this-synth-val this-synth-val)))

(defn- this-synth-params [params] (flatten (for [[k v] params] [k (get-value first k v)])))

(defn- next-synth-params [params] (into {} (for [[k v] params] [k (get-value next k v)])))

(defn- get-pattern-event
  [[prev-event pattern]]
  ; Get next-time from previous event so we know when to play this pattern event
  (let [{time :next-time stage-dur :stage-dur} prev-event
        {:keys [name synth params]} @pattern
        this-synth (this-synth synth)
        this-params (this-synth-params params)
        this-dur (get-value first (:dur params))
        next-synth (get-value next synth)
        next-params (next-synth-params params)]
    ; Keep playing pattern until one of the params is nil
    (if (not-any? nil? this-params)
      (do
        (log/debug "Current event for pattern:" name ", time:" time ", synth:" (get-in this-synth [:sdef :name]) ", params:" this-params)
        ; Swap in next events in pattern
        (swap! pattern assoc :synth next-synth :params next-params)
        ; Pass back this event and pattern so we can iterate over this function and create a lazy sequence
        [{:synth     this-synth
          :params    this-params
          :time      time
          :next-time (+ time this-dur)
          :stage-dur (+ stage-dur this-dur)}
         pattern])

      ; Return nil if pattern is complete indicating that pattern should stop playing
      (do
        (log/debug "Pattern complete:" name)
        nil))))

(defn- is-not-stage-complete?
  [staging-period event]
  ; Stage is complete if next event is nil OR total duration of events in this stage has gone beyond staging period
  (-> (or (nil? event) (>= (:stage-dur event) staging-period))
      not))

(defn- get-pattern-events
  [time pattern]
  ; If pattern is being reset to original values, we can safely do that here since are between generation of events
  (if (:reset-flag @pattern) (swap! pattern reset-pattern))

  ; Merge any queued changes into pattern before generating new events. We can safely do this here since we are between
  ; generation of events
  (swap! pattern dequeue-param-changes)

  ; Get pattern events that go in current stage (length of stage defined by pattern's staging-period). We keep the time
  ; of the next event in pattern in :next-time value and the total duration of events in this stage in :stage-dur value
  ; within pattern.
  (let [staging-period (get-in @pattern [:params :staging-period])
        ; For first event in this stage, call get-pattern-event function using a previous event that declares
        ; the "next time" to be at time of first event and staging duration reset to 0
        [first-event] (get-pattern-event [{:next-time time :stage-dur 0} pattern])
        ; Set up lazy sequence of events remaining in pattern
        lazy-events (iterate get-pattern-event [first-event pattern])
        ; Split up lazy list into events in this stage and those in future stages
        [head tail] (split-with (partial is-not-stage-complete? staging-period) (map first lazy-events))]
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
    (ot/at time (apply ot/snd "/s_new" synth-name -1 0 0 (map convert-for-s_new params)))
    (if-not (nil? synth) (log/warn "Unknown synth:" synth))))

(defn- do-pattern-events
  [events]
  (doseq [{:keys [synth params time]} events] (do-pattern-event time synth params))
  events)

(defn- setup-next-stage
  [f pattern-key {:keys [next-time]}]
  ; If pattern has not stopped playing, then set up next stage to be run on a new thread when next event in pattern starts
  (when-not (nil? next-time) (u/apply-by next-time (f next-time pattern-key))))

(defn- play-pattern
  [time pattern-key]
  ; Play events in this stage and set up next stage to be run on a separate thread at first time of that stage's events
  (->> (get-pattern pattern-key)
       (get-pattern-events time)
       do-pattern-events
       last
       (setup-next-stage play-pattern pattern-key)))

(defn- create-pattern
  [pattern-key {:keys [synth params]}]
  ; Each pattern is an atom so we can manage the workflow of the pattern across threads of execution while pattern events are generated.
  ; All pattern atoms are stored in a containing atom called "patterns " so that we can manage all the created patterns across threads.
  ; The new-params property of the pattern hash allows changing of the pattern as the pattern is being executed; when pattern changes
  ; need to occur, the changes are kept in new-params until it is safe to merge them during execution of the pattern.
  (let [merged-params (merge dflt-pattern-params params)]
    (->> (atom {:name (name pattern-key) :synth synth :params merged-params :new-params {} :orig-synth synth :orig-params merged-params})
         (swap! patterns assoc pattern-key))
    (log/debug "Created pattern" pattern-key)
    true))




;
; Public API
;
(defn reset-pattern!
  "Resets pattern to original synth and params values. Useful for restarting patterns that have stopped due to some value in
  a sequence becoming nil."
  [pattern-key]
  (-> (get-pattern pattern-key)
      (swap! assoc :reset-flag true))
  true)

(defn current-value
  "Returns the current value of the given parameter within the pattern"
  [pattern-key param-key]
  (-> (get-pattern pattern-key)
      deref
      (get-in [:params param-key])))

(defn init
  "Create new patterns identified by the given keys. Uses the given Overtone synth to produce sounds by reading the params map during
  each cycle. Each value in the params map can be a lazy sequence or a constant value. If the value (either in a lazy sequence or constant)
  is a keyword, then the sound-param multi-method is used to obtain the value for the current cycle; this is so that busses, effects, etc
  can be set in each pattern event.

  Patterns are lazy, so we need to realize them to generate their events. Pattern events are created together in a single thread until the
  staging-period period has elapsed in order to cut down on the number of threads that are needed to generate the pattern events. This is
  especially useful with microsound-based patterns.

  The paramters depend on the synth definition. However, two special parameters are used when playing the cycle:

  :dur - Duration of each event in the pattern in milliseconds. Default: 1000ms
  :staging-period - Duration of of each staging of pattern events, in milliseconds. Default: 5000ms

  Example params map:
  {:out-bus    :reverb1
   :freq       (cycle [440 220])
   :sustain    0.1
   :pan        (cycle [-1 0 1])
   :amp        0.1
   :dur        25}"
  [patterns]
  (doseq [[key pattern] patterns] (create-pattern key pattern))
  (log/info "Finished patterns init:" (pattern-keys))
  true)


; Sound event handling
(defmulti handle-event
          ; TODO API Doc for handle-event
          ""
          (fn [_time [event-type instr-key]]
            (->> (if (pattern? instr-key) "-pat" "-instr")
                 (str (name event-type))
                 keyword)))

(defmethod handle-event :default [_time [event-type]] (log/error "Unknown sound event" event-type))

(defmethod handle-event :play-pat
  [time [_event-type pattern-key]]
  (u/apply-by time (do
                     (log/info "Starting pattern" pattern-key)
                     (do
                       ; Reset play-param in case it was set to nil to stop the pattern previously (see stop function)
                       (enqueue-param-changes pattern-key play-param-key true)
                       (play-pattern time pattern-key))))
  true)

(defmethod handle-event :stop-pat
  [time [_event-type pattern-key]]
  ; By setting one of the pattern params to nil, the pattern stops on next read in cycle (see get-pattern-event func)
  ; We use a special pattern param as not to conflict with any params in pattern set by user
  (u/apply-by time (do
                     (log/info "Stopping pattern" pattern-key)
                     (enqueue-param-changes pattern-key play-param-key nil)))
  true)

(defmethod handle-event :set-pat
  [time [_event-type pattern-key & params]]
  (u/apply-by time (apply enqueue-param-changes pattern-key params))
  true)





(comment
  (if (ot/server-disconnected?) (ot/connect-external-server 4445))

  (ot/defsynth gabor [out-bus [0 :ir] freq [440 :ir] sustain [1 :ir] pan [0.0 :ir] amp [0.1 :ir] width [0.25 :ir]]
               (let [env (ot/lf-gauss:ar sustain width :loop 0 :action ot/FREE)
                     half-pi (* 0.5 (. Math PI))
                     son (* env (ot/f-sin-osc:ar freq half-pi))]
                 (ot/offset-out:ar out-bus (ot/pan2:ar son pan amp))))

  (init {:gabor1
         {:synth  gabor
          :params {:out-bus        0
                   :freq           (cycle [440 220])
                   :sustain        0.1
                   :pan            0.0
                   :amp            0.1
                   :width          0.25
                   :dur            1000
                   :staging-period 2000}}})
  (handle-event (ot/now) [:play :gabor1])
  (enqueue-param-changes :gabor1 :dur 25)
  (enqueue-param-changes :gabor1 :dur 1000)
  (enqueue-param-changes :gabor1 :sustain (map #(/ % (current-value :gabor :freq)) (range 10 0 -1)))
  (handle-event (ot/now) [:stop :gabor1]))