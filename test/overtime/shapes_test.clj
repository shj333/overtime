(ns overtime.shapes-test
  (:require [clojure.test :refer :all]
            [overtime.shapes :as shp]
            [overtone.sc.envelope :as ot]))

(defn- rounded-signal
  [env length]
  (->> (shp/env->signals env length)
       (map #(Math/round (* 1000.0 %)))))

(deftest env->signal
  (testing "env-sine to signal"
    (is (= (rounded-signal (ot/env-sine) 20)
           '(0 27 105 227 377 541 701 839 940 993 993 940 839 701 541 377 227 105 27 0))))
  (testing "env-perc to signal"
    (is (= (rounded-signal (ot/env-perc 0.1 0.9) 10)
           '(0 951 573 342 202 116 63 31 12 0))))
  (testing "env-triangle to signal"
    (is (= (rounded-signal (ot/env-triangle) 10)
           '(0 222 444 667 889 889 667 444 222 0))))
  (testing "env-quasi guass to signal"
    (is (= (rounded-signal (ot/envelope [0 1 1 0] [0.33 0.34 0.33] :sin) 10)
           '(0 255 759 1000 1000 1000 1000 759 255 0)))))


