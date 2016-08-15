(ns overtime.utils)

(defn check-nil
  [val & labels]
  (if (nil? val) (->> labels
                      (clojure.string/join " ")
                      (println "WARN: Value is nil:")))
  val)