(ns overtime.bus-control
  (:require [overtone.core :as ot]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private busses (atom {}))

(defn- make-bus
  [bus-key bus-type num-chans]
  (log/debug "Creating" bus-type "bus for key" bus-key)
  (let [f (case bus-type
            :control ot/control-bus
            :audio ot/audio-bus
            (throw (Exception. (str "Unknown bus type: " bus-type))))]
    (f num-chans)))

(defn- init-busses
  [bus-category busses-data]
  (log/debug "Creating busses for category" bus-category "=>" busses-data)
  (into {} (for [[bus-key [bus-type num-chans]] busses-data] [bus-key (make-bus bus-key bus-type num-chans)])))

(defn init
  [bus-map]
  (->> (into {} (for [[bus-category busses-data] bus-map] [bus-category (init-busses bus-category busses-data)]))
       (reset! busses))
  (log/debug "Finished init for busses")
  true)

(defn bus [bus-category bus-key] (u/check-nil (get-in @busses [bus-category bus-key]) "Bus" bus-category bus-key))

(defn bus-categories [] (keys @busses))

(defn bus-keys [bus-category] (keys (u/check-nil (bus-category @busses) "Bus Category" bus-category)))

(defn add-bus
  [bus-category bus-key bus-type num-chans]
  (log/debug "Adding" bus-type "bus" bus-key "to" bus-category "with" num-chans "channels")
  (swap! busses assoc-in [bus-category bus-key] (make-bus bus-key bus-type num-chans)))

(defn- merge-new-busses
  [cur-busses bus-category new-busses]
  (->> new-busses
       (merge (get cur-busses bus-category {}))
       (assoc cur-busses bus-category)))

(defn add-busses
  [bus-category bus-keys bus-type num-chans]
  (log/debug "Adding" bus-type "busses for keys" bus-keys "to" bus-category "with" num-chans "channels")
  (->> (into {} (for [bus-key bus-keys] [bus-key (make-bus bus-key bus-type num-chans)]))
       (swap! busses merge-new-busses bus-category)))


(comment
  (bus/init {:cat1 {:test-cat1-bus1 [:control 1]
                    :test-cat1-bus2 [:control 2]}
             :cat2 {:test-cat2-bus1 [:audio 1]
                    :test-cat2-bus2 [:audio 2]}}))