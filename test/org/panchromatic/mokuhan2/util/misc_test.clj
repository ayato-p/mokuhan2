(ns org.panchromatic.mokuhan2.util.misc-test
  (:require [org.panchromatic.mokuhan2.util.misc :as sut]
            [clojure.test :as t]))

(t/deftest deep-merge-test
  (t/is (= {:x 1}
           (sut/deep-merge {:x 1} {})))

  (t/is (= {:x 1}
           (sut/deep-merge {} {:x 1})))

  (t/is (= {:x 1 :y 2}
           (sut/deep-merge {:x 1} {:y 2})))

  (t/is (= {:x {:y 1} :z 2}
           (sut/deep-merge {:x {:y 1}} {:z 2})))

  (t/is (= {:x {:y 1 :z 2}}
           (sut/deep-merge {:x {:y 1}} {:x {:z 2}})))

  (t/is (= {:x {:y 1}}
           (sut/deep-merge {:x {:y 1}} nil)))

  (t/is (= {:x {:y 2}}
           (sut/deep-merge {:x {:y 1}} {:x {:y 2}}))))
