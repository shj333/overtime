(ns overtime.core
  (:require [overtone.core :as ot]
            [overtone.sc.machinery.server.comms :as comms]
            [overtime.busses :as bus]
            [overtime.groups :as grp]
            [overtime.instruments :as instr]
            [overtime.microsounds :as micro]
            [overtime.patterns :as pat]
            [overtime.sounds :as snd]
            [clojure.tools.logging :as log]))


(defn- check-keys
  [sounds patterns]
  (if-let [common-keys (keys (select-keys sounds (keys patterns)))]
    (log/error "Sound Definitions and Patterns must have unique keys, but these keys are common to both:" common-keys)))

(defn- define-synths
  [def-synths-f]
  (comms/with-server-sync def-synths-f)
  (log/info "Finished defining synths"))

(defn- get-start-times
  [section-lengths]
  (let [session-start (+ (ot/now) 2000)
        lengths (take (dec (count section-lengths)) (reductions + (map #(* 1000 %) section-lengths)))
        start-times (cons 0 lengths)]
    (map #(+ session-start %) start-times)))

(defn- event-time
  [start-time time]
  (->> (* time 1000.0)
       Math/round
       (+ start-time)))

(defn- play-section
  [{:keys [name events start-time]} opts]
  (ot/apply-by start-time #(log/info "Starting section" name))
  (doseq [event-data events]
    (-> (event-time start-time (first event-data))
        (instr/handle-event (rest event-data)))))




;
; Public API
;
(defn init
  "Initialize Overtime system by connecting to SC Server and initializing busses, SC groups, synths, patterns and microsounds. The keyed
  parameters are:
    - busses
    - micro
    - def-synths-f (optional)
    - sounds
    - patterns
    - sc-port (defaults to 4445)"
  [{:keys [busses micro def-synths-f sounds patterns sc-port] :or {sc-port 4445}}]
  (if (ot/server-disconnected?) (ot/connect-external-server sc-port))
  (check-keys sounds patterns)
  (grp/init)
  (bus/init busses)
  (micro/init micro)
  (if def-synths-f (define-synths def-synths-f))
  (snd/init sounds)
  (pat/init patterns)
  (log/info "Overtime init complete")
  true)

(defn play-sections
  ; TODO API doc for play-sections
  "Play music defined by given data"
  ([sections-data] (play-sections sections-data {}))
  ([sections-data opts]
    ; TODO Add option to start at given section
    ; TODO Add option to skip section
   (let [start-times (get-start-times (map :length sections-data))
         sects-data-with-starts (map #(assoc %1 :start-time %2) sections-data start-times)]
     (doseq [section-data sects-data-with-starts] (play-section section-data opts)))))
