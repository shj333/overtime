(ns overtime.utils
  (:require [clojure.tools.logging :as log]))

(defn check-nil
  [val & labels]
  (if (nil? val) (->> labels
                      (clojure.string/join " ")
                      (log/error "Value is nil:")))
  val)