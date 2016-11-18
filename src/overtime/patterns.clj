(ns overtime.patterns
  (:require [overtone.core :as ot]
            [overtime.shapes :as shp]
            [overtime.sounds :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private patterns (atom {}))
(defonce ^:private dflt-pattern-params {:dur 1000 :staging-period 5000})
(defonce ^:private play-param-key :play-pattern$)


;
; Pattern Utils
;
(defn- get-pattern [pattern-key] (u/check-nil (pattern-key @patterns) "Pattern" pattern-key))

(defn- pattern-keys [] (keys @patterns))

(defn- print-params
  [params]
  ; Values may be a lazy sequence which must not be evaluated
  (->> (for [[k v] params] (str k " => " (if (seq? v) "seq" v)))
       (clojure.string/join ", ")))


;
; Event Param Computation
;
(defprotocol PatternParam
  (set-pattern-param [p-param key val])
  (get-pattern-param [p-param time]))

(defn- pattern-param? [param] (satisfies? PatternParam param))

(defn- setup-param
  [pat-param key val]
  (if (pattern-param? pat-param)
    (set-pattern-param pat-param key val)
    pat-param))

(defn- setup-params
  [params key val]
  (into {} (for [[k pat-param] params] [k (setup-param pat-param key val)])))

(defn- get-value
  [key val time]
  (cond
    ; If value supports PatternParam protocol, evaluate with the protocol
    (pattern-param? val) (get-pattern-param val time)
    ; If value is a lazy sequence, get first one in sequence
    (seq? val) (first val)
    ; If a keyword was given, this is a sound parameter
    key (snd/sound-param key val)
    ; Otherwise use the param value as is
    :else val))

(defn- this-synth
  [synth]
  ; Dereference synth value if synth is defined as a var
  (let [this-synth-val (get-value nil synth nil)]
    (if (var? this-synth-val) @this-synth-val this-synth-val)))

(defn- this-synth-params [time params] (into {} (for [[k v] params] [k (get-value k v time)])))


(defn- get-next-values
  [val]
  ; If value is a lazy sequence, get rest of sequence, otherwise just use value as is
  (if (seq? val) (next val) val))

(defn- next-synth [synth] (get-next-values synth))

(defn- next-synth-params [params] (into {} (for [[k v] params] [k (get-next-values v)])))


;
; Pattern Stream Modification
;
(defn- enqueue-param-changes
  [pattern-key & {:as params}]
  (let [pattern (get-pattern pattern-key)]
    (log/debug "Enqueueing pattern changes for" (:name @pattern) "to:" (print-params params))
    (swap! pattern update-in [:new-params] merge params)
    true))

(defn- update-pattern
  [derefed-pattern time]
  (log/debug "Changing pattern" (:name derefed-pattern) "to:" (print-params (:new-params derefed-pattern)))
  ; Merge in queued param changes and then reset queue to empty
  (-> derefed-pattern
      (update-in [:params] merge (setup-params (:new-params derefed-pattern) :start-time time))
      (assoc :new-params {})))

(defn- dequeue-param-changes
  ; Pattern is de-referenced since this function is called by swap!
  [derefed-pattern time]
  (if (empty? (:new-params derefed-pattern))
    derefed-pattern
    (update-pattern derefed-pattern time)))

(defn- reset-pattern
  ; Pattern is de-referenced since this function is called by swap!
  [derefed-pattern]
  (log/debug "Resetting pattern" (:name derefed-pattern) "to original synth and params")
  (assoc derefed-pattern :synth (:orig-synth derefed-pattern) :params (:orig-params derefed-pattern) :reset-flag false))

(defn- set-start-time
  ; Pattern is de-referenced since this function is called by swap!
  [derefed-pattern time]
  (assoc derefed-pattern :params (setup-params (:params derefed-pattern) :start-time time)))


;
; Pattern Event Generation
;
(defn- gen-event
  [prev-event pattern this-params]
  (let [{:keys [name synth params]} @pattern
        ; Get next-time from previous event so we know when to play this pattern event
        {time :next-time stage-elapsed :stage-elapsed} prev-event
        this-synth (this-synth synth)
        this-dur (:dur this-params)
        flat-params (flatten (seq this-params))
        next-synth (next-synth synth)
        next-params (next-synth-params params)]
    ; Swap in next events in pattern
    (log/debug "Current event for pattern:" name ", time:" time ", synth:" (get-in this-synth [:sdef :name]) ", params:" flat-params)
    (swap! pattern assoc :synth next-synth :params next-params)

    {:synth         this-synth
     :params        flat-params
     :time          time
     :next-time     (+ time this-dur)
     :stage-elapsed (+ stage-elapsed this-dur)}))

(defn- gen-stop-event
  [pattern]
  ; Return nil if pattern is complete indicating that pattern should stop playing
  (log/debug "Pattern complete:" (:name @pattern))
  nil)

(defn- get-pattern-event
  [[prev-event pattern]]
  ; Keep generating pattern events until one of the pattern params is nil
  (let [this-params (this-synth-params (:next-time prev-event) (:params @pattern))]
    (if (some nil? (vals this-params))
      (gen-stop-event pattern)

      ; Pass back event data and pattern so we can iterate over this function and create a lazy sequence
      [(gen-event prev-event pattern this-params) pattern])))

(defn- is-not-stage-complete?
  [staging-period event]
  ; Stage is complete if next event is nil OR elapsed time of events in this stage has gone beyond staging period
  (-> (or (nil? event) (>= (:stage-elapsed event) staging-period))
      not))

(defn- get-pattern-events
  [time pattern]
  ; If pattern is being reset to original values, we can safely do that here since are between generation of events
  (if (:reset-flag @pattern) (swap! pattern reset-pattern))

  ; Merge any queued changes into pattern before generating new events. We can safely do this here since we are between
  ; generation of events
  (swap! pattern dequeue-param-changes time)

  ; Get pattern events that go in current stage (length of stage defined by pattern's staging-period). We keep the time
  ; of the next event in pattern in :next-time and the elapsed time of events in this stage in :stage-elapsed.
  (let [staging-period (get-in @pattern [:params :staging-period])
        ; For first event in this stage, call get-pattern-event function using a previous event that declares
        ; the "next time" to be at time of first event and staging duration reset to 0
        [first-event] (get-pattern-event [{:next-time time :stage-elapsed 0} pattern])
        ; Set up lazy sequence of events remaining in pattern
        lazy-events (iterate get-pattern-event [first-event pattern])
        ; Split up lazy list into events in this stage and those in future stages
        [head tail] (split-with (partial is-not-stage-complete? staging-period) (map first lazy-events))]
    ; We need to take the first event of tail since it's already been consumed within lazy sequence in management above
    (concat head (take 1 tail))))



;
; Pattern Control
;
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


;
; Public API
;
(defn create-pattern
  "Create a new pattern identified by the given key. Uses the given Overtone synth to produce sounds by reading the params map during
  each cycle. The synth param can be a lazy sequence that is evaluated with each pattern event generation. Each value in the params map
  can also be a lazy sequence (or a constant value). If the value (either in a lazy sequence or constant) is a keyword, then the
  overtime.sounds/sound-param function is used to obtain the value for the current cycle ; this is so that busses, effects, etc can
  be set in each pattern event.

  Patterns are lazy, so we need to realize them to generate their events. Pattern events are created together in a single thread until the
  staging-period period has elapsed in order to cut down on the number of threads that are needed to generate the pattern events. This is
  especially useful with microsound-based patterns.

  The paramters depend on the synth definition. However, two special parameters are used when playing the cycle:

  :dur - Duration of each event in the pattern in milliseconds. Default: 1000ms
  :staging-period - Duration of of each staging of pattern events, in milliseconds. Default: 5000ms

  Example pattern:
  {:synth  #'syn/gabor
   :params {:out            0
            :freq           (cycle 1000 1500 2000)
            :sustain        (cycle 0.02 0.04 0.06)
            :pan            (cycle -1.0 0.0 1.0)
            :amp            0.2
            :dur            500
            :staging-period 1000}}"
  [pattern-key {:keys [synth params]}]
  ; Each pattern is an atom so we can manage the workflow of the pattern across threads of execution while pattern events are generated.
  ; All pattern atoms are stored in a containing atom called "patterns " so that we can manage all the created patterns across threads.
  ; The new-params property of the pattern hash allows changing of the pattern as the pattern is being executed; when pattern changes
  ; need to occur, the changes are kept in new-params until it is safe to merge them during execution of the pattern.
  (let [merged-params (-> (merge dflt-pattern-params params)
                          (setup-params :pattern-key pattern-key))]
    (->> (atom {:name (name pattern-key) :synth synth :params merged-params :new-params {} :orig-synth synth :orig-params merged-params})
         (swap! patterns assoc pattern-key))
    (log/debug "Created pattern" pattern-key)
    true))

(defn init
  "Create new patterns identified by the given keys. Uses the given Overtone synth to produce sounds by reading the params map during
  each cycle. The synth param can be a lazy sequence that is evaluated with each pattern event generation. Each value in the params map
  can also be a lazy sequence (or a constant value). If the value (either in a lazy sequence or constant) is a keyword, then the
  overtime.sounds/sound-param function is used to obtain the value for the current cycle ; this is so that busses, effects, etc can
  be set in each pattern event.

  Patterns are lazy, so we need to realize them to generate their events. Pattern events are created together in a single thread until the
  staging-period period has elapsed in order to cut down on the number of threads that are needed to generate the pattern events. This is
  especially useful with microsound-based patterns.

  The paramters depend on the synth definition. However, two special parameters are used when playing the cycle:

  :dur - Duration of each event in the pattern in milliseconds. Default: 1000ms
  :staging-period - Duration of of each staging of pattern events, in milliseconds. Default: 5000ms

  Example pattern:
  {:synth  #'syn/gabor
   :params {:out            0
            :freq           (cycle 1000 1500 2000)
            :sustain        (cycle 0.02 0.04 0.06)
            :pan            (cycle -1.0 0.0 1.0)
            :amp            0.2
            :dur            500
            :staging-period 1000}}"
  [patterns]
  (doseq [[key pattern] patterns] (create-pattern key pattern))
  (log/info "Finished patterns init:" (pattern-keys))
  true)

(defn pattern?
  "Returns true if the given key denotes a pattern object (as opposed to a synth instrument object)"
  [key]
  (contains? @patterns key))

(defn start-pattern
  "Start playing the given pattern with the given start time"
  [time pattern-key]
  ; Reset play-param in case it was set to nil to stop the pattern previously (see stop function)
  (enqueue-param-changes pattern-key play-param-key true)

  ; Set start time in any PatternParam's
  (swap! (get-pattern pattern-key) set-start-time time)

  (log/info "Starting pattern" pattern-key)
  (play-pattern time pattern-key))

(defn stop-pattern
  "Stop playing of the given pattern"
  [pattern-key]
  ; By setting one of the pattern params to nil, the pattern stops on next read in cycle (see get-pattern-event func)
  ; We use a special pattern param as not to conflict with any params in pattern set by user
  (log/info "Stopping pattern" pattern-key)
  (enqueue-param-changes pattern-key play-param-key nil))

(defn change-pattern
  "Change the parameters of the given pattern"
  [pattern-key params]
  (log/info "Setting params" (u/print-param-keys params) "for pattern" pattern-key)
  (apply enqueue-param-changes pattern-key params))

(defn reset-pattern!
  "Resets pattern to original synth and params values. Useful for restarting patterns that have stopped due to some value in
  a sequence becoming nil."
  [pattern-key]
  (log/info "Resetting pattern" pattern-key)
  (-> (get-pattern pattern-key)
      (swap! assoc :reset-flag true))
  true)

(defn current-value
  "Returns the current value of the given parameter within the pattern"
  [pattern-key param-key]
  (let [val (-> (get-pattern pattern-key)
                deref
                (get-in [:params param-key]))]
    ; FIXME Function current-value isn't quite right since it ignores time
    (get-value nil val nil)))



(defrecord PatternEnv [pattern-key start-time env-stages is-looped]
  PatternParam
  (set-pattern-param [pat-env key val]
    (-> (merge pat-env {key val})
        map->PatternEnv))
  (get-pattern-param [pat-env cur-time]
    (if (some nil? (vals pat-env)) (log/error "PatternEnv has nil values: " pat-env))
    (let [elapsed-time (/ (- cur-time start-time) 1000.0)
          tot-env-time (shp/env-total-dur env-stages)]
      (if (or is-looped (< elapsed-time tot-env-time))
        (shp/signal-at env-stages (mod elapsed-time tot-env-time))
        nil))))

(defn pattern-env [env is-looped] (PatternEnv. nil nil (shp/env-stages env) is-looped))




(comment
  (do
    (if (ot/server-disconnected?) (ot/connect-external-server 4445))

    (ot/defsynth gabor [out-bus [0 :ir] freq [440 :ir] sustain [1 :ir] pan [0.0 :ir] amp [0.1 :ir] width [0.25 :ir]]
                 (let [env (ot/lf-gauss:ar sustain width :loop 0 :action ot/FREE)
                       half-pi (* 0.5 (. Math PI))
                       son (* env (ot/f-sin-osc:ar freq half-pi))]
                   (ot/offset-out:ar out-bus (ot/pan2:ar son pan amp))))

    (init {:gabor1
           {:synth  gabor
            :params {:out-bus        0
                     ; :freq           (cycle [440 220])
                     :freq            (pattern-env (ot/envelope [440 880] [4] :exp) false)
                     :sustain        0.1
                     :pan            0.0
                     :amp            0.1
                     :width          0.25
                     :dur            50
                     :staging-period 2000}}}))
  (start-pattern (ot/now) :gabor1)
  (change-pattern :gabor1 [:freq (pattern-env (ot/envelope [440 880 660] [4 3] :exp) false)])
  (change-pattern :gabor1 [:dur 25])
  (change-pattern :gabor1 [:dur 1000])
  (change-pattern :gabor1 [:sustain (map #(/ % (current-value :gabor1 :freq)) (range 10 0 -1))])
  (reset-pattern! :gabor1)
  (stop-pattern :gabor1)

  (shp/env-stages (ot/envelope [440 880 660] [4 3] :exp))
  (shp/view-env (ot/envelope [440 880 660] [4 3] :exp) 700))