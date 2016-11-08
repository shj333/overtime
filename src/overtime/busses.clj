(ns overtime.busses
  (:require [overtone.core :as ot]
            [overtime.sounds :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private busses (atom {}))

(defn- make-bus
  [key bus-type num-chans]
  (log/debug "Creating" bus-type "bus for key" key)
  (let [f (case bus-type
            :control ot/control-bus
            :audio ot/audio-bus
            (throw (Exception. (str "Unknown bus type: " bus-type))))]
    (f num-chans (name key))))

(defn- init-busses
  [category busses-data]
  (log/debug "Creating busses for category" category "=>" busses-data)
  (into {} (for [[key [bus-type num-chans]] busses-data] [key (make-bus key bus-type num-chans)])))

(defn bus-categories [] (keys @busses))

(defn bus-keys
  [category]
  (-> (u/check-nil (category @busses) "Bus Category" category)
      keys
      sort))

(defn bus [category key] (u/check-nil (get-in @busses [category key]) "Bus" category key))

(defn add-bus
  [category key bus-type num-chans]
  (log/debug "Adding" bus-type "bus" key "to" category "with" num-chans "channels")
  (swap! busses assoc-in [category key] (make-bus key bus-type num-chans)))

(defn- merge-new-busses
  [cur-busses category new-busses]
  (->> new-busses
       (merge (get cur-busses category {}))
       (assoc cur-busses category)))

(defn add-busses
  [category keys bus-type num-chans]
  (log/debug "Adding" bus-type "busses for keys" keys "to" category "with" num-chans "channels")
  (->> (into {} (for [key keys] [key (make-bus key bus-type num-chans)]))
       (swap! busses merge-new-busses category)))

(defn init
  [bus-map]
  (->> (into {} (for [[category busses-data] bus-map] [category (init-busses category busses-data)]))
       (reset! busses))
  (log/info "Finished busses init")
  (doseq [category (bus-categories)] (log/info "Bus category" category ":" (bus-keys category)))
  true)


(defmethod snd/sound-param-val :in [_type val] (bus :fx val))
(defmethod snd/sound-param-val :out [_type val] (bus :fx val))
(defmethod snd/sound-param-val :control-bus [_type val] (bus :control val))
(defmethod snd/sound-param-val :trigger-bus [_type val] (bus :trigger val))
(defmethod snd/sound-param-val :pan-bus [_type val] (bus :pan val))

(comment
  (bus/init {:cat1 {:test-cat1-bus1 [:control 1]
                    :test-cat1-bus2 [:control 2]}
             :cat2 {:test-cat2-bus1 [:audio 1]
                    :test-cat2-bus2 [:audio 2]}}))
