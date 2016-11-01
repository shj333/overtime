(ns overtime.utils
  (:require [clojure.tools.logging :as log]))

(defn check-nil
  [val & labels]
  (if (nil? val) (->> labels
                      (clojure.string/join " ")
                      (log/error "Value is nil:")))
  val)

(defn num-lin-lin
  ([x in-min in-max out-min out-max] (num-lin-lin x in-min in-max out-min out-max :min-max))
  ([x in-min in-max out-min out-max clip]
   (cond (and (clip #{:min-max :min}) (<= x in-min)) out-min
         (and (clip #{:min-max :max}) (>= x in-max)) out-max
         true (+ out-min (* (- out-max out-min) (/ (- x in-min) (- in-max in-min)))))))

(defmacro apply-by
  [time body]
  `(ot/apply-by ~time #(try ~body
                            (catch Exception e# (log/error (str "Caught exception in apply-by " ~time ", " '~body ": ") e#)))))

(defn print-param-keys
  [params]
  (->> (take-nth 2 params)
       (clojure.string/join ", ")))
