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
; Pattern Param Computation
;
(defprotocol PatternParam
  (set-start-time [p-param time])
  (get-p-param-val [p-param param-key time])
  (get-p-param-next [p-param]))

(extend-type Object
  PatternParam
  (set-start-time [p-param _time] p-param)
  (get-p-param-val [p-param param-key _time]
    ; If value is a lazy sequence, get first one in sequence, otherwise get sound parameter
    (if (seq? p-param)
      (first p-param)
      (snd/sound-param param-key p-param)))
  (get-p-param-next [p-param] (if (seq? p-param) (next p-param) p-param)))

(extend-type nil
  PatternParam
  (set-start-time [p-param _time] p-param)
  (get-p-param-val [_p-param _param-key _time] nil)
  (get-p-param-next [_p-param] nil))

(defrecord EnvParam [env-stages total-env-time is-looped transform-f start-time]
  PatternParam
  (set-start-time [env-param time]
    (-> (assoc env-param :start-time time)
        map->EnvParam))
  (get-p-param-val [env-param _param-key cur-time]
    (if (some nil? (vals env-param)) (log/error "EnvParam has nil values: " env-param))
    (log/debug env-param ", cur-time: " cur-time)
    (let [elapsed-time (/ (- cur-time start-time) 1000.0)]
      (if (or is-looped (< elapsed-time total-env-time))
        (->> (mod elapsed-time total-env-time)
             (shp/signal-at env-stages)
             transform-f)
        nil)))
  (get-p-param-next [p-param] p-param))

(defn env-param
  ([env is-looped] (env-param env is-looped identity))
  ([env is-looped transform-f]
   (let [env-stages (shp/env-stages env)
         total-env-time (shp/env-total-dur env-stages)]
     (EnvParam. env-stages total-env-time is-looped transform-f nil))))

(defmethod print-method EnvParam
  [pat-env ^java.io.Writer w]
  (.write w (pr-str "EnvParam [stages:" (:env-stages pat-env)
                    "total time:" (:total-env-time pat-env)
                    "looped:" (:is-looped pat-env)
                    "start:" (:start-time pat-env)
                    "]")))

; TODO Another record ParamStochastic similar to EnvParam that uses envelope to get chance of event being
; TODO played (env vals 0.0-1.0, where 0 means rest and 1.0 means play and otherwise pct of time that event
; TODO is played (see Roads Pulsar Synth paper). Maybe not needed now that we have transform-f in EnvParam?

; TODO Figure out where burst-seq func should live (used for setting burst ratio of pattern -- see Roads Pulsar Synth paper)
; TODO Maybe impl this as a record BurstRatioParam that impls PatternParam protocol?
(defn burst-seq
  [b r]
  (cycle (concat (repeat b 0) (repeat r 1))))


(defn- init-pat-params
  [pat-params time]
  (into {} (for [[k pat-param] pat-params] [k (set-start-time pat-param time)])))

; TODO Instead of process-lazy-list, make everything a PatternParam protocol, use repeat function and add get-next-params function to proctocol
(defn- process-lazy-list [f val] (if (seq? val) (f val) val))

; TODO Should synth just be part of params instead of separate functionality?
(defn- this-synth
  [synth]
  (let [this-synth-val (process-lazy-list first synth)]
    ; Dereference synth value if synth is defined as a var
    (if (var? this-synth-val) @this-synth-val this-synth-val)))

(defn- this-synth-params
  [time params]
  (into {} (for [[param-key p-param] params] [param-key (get-p-param-val p-param param-key time)])))

(defn- next-synth [synth] (process-lazy-list next synth))

(defn- next-synth-params [params] (into {} (for [[param-key p-param] params] [param-key (get-p-param-next p-param)])))


;
; Pattern Stream Modification
;
(defn- enqueue-param-changes
  [pattern-key & {:as params}]
  (let [pattern (get-pattern pattern-key)]
    (log/debug "Enqueueing pattern changes for" (:name @pattern) "to:" (print-params params))
    (swap! pattern update :new-params merge params)
    true))

(defn- update-pattern
  [derefed-pattern time]
  (log/debug "Changing pattern" (:name derefed-pattern) "to:" (print-params (:new-params derefed-pattern)))
  ; Set start time in new pattern params in case any are a PatternParam protocol
  (let [new-params (init-pat-params (:new-params derefed-pattern) time)]
    ; Merge in queued param changes and then reset queue to empty
    (-> derefed-pattern
        (update :params merge new-params)
        (assoc :new-params {}))))

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

    ; Pass back event data and pattern so we can iterate over this function and create a lazy sequence
    [{:synth         this-synth
      :params        flat-params
      :rest          (:rest this-params)
      :time          time
      :next-time     (+ time this-dur)
      :stage-elapsed (+ stage-elapsed this-dur)}
     pattern]))

(defn- gen-stop-event
  [pattern-name]
  ; Return nil if pattern is complete indicating that pattern should stop playing
  (log/debug "Pattern complete:" pattern-name)
  nil)

(defn- get-pattern-event
  [[prev-event pattern]]
  ; Keep generating pattern events until one of the pattern params is nil
  (let [this-params (this-synth-params (:next-time prev-event) (:params @pattern))]
    (if (some nil? (vals this-params))
      (gen-stop-event (:name @pattern))
      (gen-event prev-event pattern this-params))))

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
  [time synth params rest]
  ; TODO Need some sort of note (API Doc?) for how :rest works within pattern
  (if-not (= 1 rest)
    ; Set up a single pattern event to be played on SC server. Use "/s_new" OSC command instead of calling synth function to cut down
    ; on overhead. This efficiency helps when dealing with microsound-based patterns.
    (if-let [synth-name (get-in synth [:sdef :name])]
      (ot/at time (apply ot/snd "/s_new" synth-name -1 0 0 (map convert-for-s_new params)))
      (if-not (nil? synth) (log/warn "Unknown synth:" synth)))))

(defn- do-pattern-events
  [events]
  (doseq [{:keys [synth params time rest]} events] (do-pattern-event time synth params rest))
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
  (let [merged-params (merge dflt-pattern-params params)]
    (->> (atom {:key pattern-key :name (name pattern-key) :synth synth :params merged-params :new-params {} :orig-synth synth :orig-params merged-params})
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
  (swap! (get-pattern pattern-key) update :params init-pat-params time)

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
  (let [p-param (-> (get-pattern pattern-key)
                deref
                (get-in [:params param-key]))]
    (get-p-param-val p-param param-key (ot/now))))



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
                     :freq           (env-param (ot/envelope [440 880] [4] :exp) false)
                     :sustain        0.1
                     :pan            0.0
                     :amp            0.1
                     :width          0.25
                     :dur            50
                     :staging-period 2000}}}))
  (start-pattern (ot/now) :gabor1)
  (change-pattern :gabor1 [:freq (env-param (ot/envelope [440 880 660] [4 3] :exp) true)])
  (change-pattern :gabor1 [:sustain (env-param (ot/envelope [0.005 0.5 0.005] [3 5] :exp) true)])
  (change-pattern :gabor1 [:dur 25])
  (change-pattern :gabor1 [:dur 1000])
  (change-pattern :gabor1 [:sustain (map #(/ % (current-value :gabor1 :freq)) (range 10 0 -1))])
  (reset-pattern! :gabor1)
  (stop-pattern :gabor1)

  (shp/env-stages (ot/envelope [440 880 660] [4 3] :exp))
  (shp/view-env (ot/envelope [440 880 660] [4 3] :exp) 700))