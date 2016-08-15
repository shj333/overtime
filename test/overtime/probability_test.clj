(ns overtime.probability-test
  (:require [clojure.test :refer :all]
            [overtime.probability :as prob]))

(defn- find-mean [& args]
  (let [seq (repeatedly #(apply prob/exp-rand args))]
    (Math/round (/ (reduce + (take 100000 seq)) 10000))))

(deftest prob-test
  (testing "exp-rand"
    (is (= 78 (find-mean 2 20)))))

