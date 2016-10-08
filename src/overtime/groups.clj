(ns overtime.groups
  (:require [overtone.core :as ot]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))

(defonce ^:private groups (atom {}))

(defn- make-groups
  []
  (let [container (ot/group "container")
        producers (ot/group "producers" :head container)
        fx (ot/group "fx" :after producers)]
    (swap! groups assoc :producers producers :fx fx)))

(defn init
  []
  (make-groups)
  (log/info "Finished groups init:" (keys @groups))
  true)

(defn group [key] (u/check-nil (key @groups) "Group" key))

