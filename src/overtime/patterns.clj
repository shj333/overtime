(ns overtime.patterns
  (:require [overtone.core :as ot]
            [clojure.tools.logging :as log]
            [overtime.utils :as u]))


(defonce ^:private patterns (atom {}))
(defonce ^:private dflt-pattern-params {:dur-per-stage 5000})

(defn- get-pattern [pattern-key] (u/check-nil (pattern-key @patterns) "Pattern" pattern-key))

(defn- get-value
  [f param]
  (if (seq? param)
    (f param)
    param))

(defn- this-synth-params
  [params]
  (flatten (for [[k v] params] [k (get-value first v)])))

(defn- next-synth-params
  [params]
  (into {} (for [[k v] params] [k (get-value next v)])))

(defn- get-pattern-event
  [[prev-val pattern]]
  (let [time (:next-time prev-val)
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
        {:synth     this-synth
         :params    this-params
         :time      time
         :next-time (+ time this-dur)
         :total-dur (+ (:total-dur prev-val) this-dur)})
      (do
        (log/debug "Pattern complete:" name)
        nil))))

(defn- get-pattern-events
  [time pattern]
  (let [first-event (get-pattern-event [{:next-time time :total-dur 0} pattern])]
    (iterate get-pattern-event first-event)))

(defn- is-not-stage-complete?
  [dur-per-stage event]
  (if (nil? event)
    false
    (< (:total-dur event) dur-per-stage)))

(defn do-pattern-at
  [time pattern-key]
  (let [pattern (get-pattern pattern-key)
        dur-per-stage (get-in pattern [:params :dur-per-stage])
        events (take-while (partial is-not-stage-complete? dur-per-stage) (get-pattern-events time pattern))
        last-event (last events)]
    (doseq [{:keys [synth params time]} events] (ot/at time (apply synth params)))
    (if-not (nil? last-event)
      (let [{:keys [next-time]} last-event]
        (u/apply-by next-time (do-pattern-at next-time pattern-key))))))

(defn make-pattern
  [pattern-key synth params]
  (let [pattern (atom {:name (name pattern-key) :synth synth :params (merge dflt-pattern-params params)})]
    (swap! patterns assoc pattern-key pattern)))

(defn change-pattern
  [pattern-key synth params]
  (let [pattern (get-pattern pattern-key)]
    (if-not (nil? synth) (swap! pattern assoc :synth synth))
    (swap! pattern update-in [:params] merge params)))

(defn pattern-value
  [pattern-key param-key]
  (let [pattern (get-pattern pattern-key)]
    (param-key (:params @pattern))))
