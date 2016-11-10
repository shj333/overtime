(ns overtime.core
  (:require [overtone.core :as ot]
            [overtone.sc.machinery.server.comms :as comms]
            [overtime.busses :as bus]
            [overtime.buffers :as buf]
            [overtime.sound-commands :as cmd]
            [overtime.groups :as grp]
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
  [{:keys [name events start-time] :or {events []}} opts]
  (ot/apply-by start-time #(log/info "Starting section" name))
  (doseq [event-data events]
    (-> (event-time start-time (first event-data))
        (cmd/do-sound-cmd (rest event-data)))))




;
; Public API
;
(defn init
  ; TODO More details in code doc for params to init function
  "Initialize Overtime system by connecting to SC Server and initializing busses, buffers SC groups, synths, patterns and microsounds. The keyed
  parameters are:
    - busses (defaults to no created busses)
    - buffers (defaults to no additional buffers)
    - micro (defaults to ???)
    - def-synths-f (defaults to no created synths)
    - sounds (defaults to no defined sounds)
    - patterns (defaults to no created patterns)
    - sc-port (defaults to 4445)"
  [{:keys [busses buffers micro def-synths-f sounds patterns sc-port] :or {busses {} buffers {} sc-port 4445}}]
  (if (ot/server-disconnected?) (ot/connect-external-server sc-port))
  (check-keys sounds patterns)
  (grp/init)
  (bus/init busses)
  (buf/init buffers)
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
   (let [start-times (get-start-times (map #(get % :length 0) sections-data))
         sects-data-with-starts (map #(assoc %1 :start-time %2) sections-data start-times)]
     (doseq [section-data sects-data-with-starts] (play-section section-data opts)))))

(defn pattern-value
  "Returns the current value of the given parameter within the pattern"
  [pattern-key param-key]
  (pat/current-value pattern-key param-key))

(defn do-sound-cmd
  ; TODO Doc for API
  ""
  [event-data]
  (cmd/do-sound-cmd event-data))
