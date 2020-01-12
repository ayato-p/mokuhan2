(ns org.panchromatic.tiny-mokuhan.reader-test
  (:require [org.panchromatic.tiny-mokuhan.reader :as sut]
            [clojure.test :as t]))

(defn- gen-rdr [s]
  (java.io.StringReader. s))

(t/deftest read-char-test
  (let [rdr (gen-rdr "Hello")]
    (t/is (= \H (sut/read-char rdr)))
    (t/is (= \e (sut/read-char rdr)))
    (t/is (= \l (sut/read-char rdr)))
    (t/is (= \l (sut/read-char rdr)))
    (t/is (= \o (sut/read-char rdr)))
    (t/is (nil? (sut/read-char rdr)))))

(t/deftest read-chars-test
  (let [rdr (gen-rdr "Hello")]
    (t/is (= [\H \e] (sut/read-chars rdr 2)))
    (t/is (= [\l \l] (sut/read-chars rdr 2)))
    (t/is (= [\o nil] (sut/read-chars rdr 2))))

  (let [rdr (gen-rdr "Hello")]
    (t/is (= [] (sut/read-chars rdr 0)))))

(t/deftest unread-char-test
  (let [rdr (sut/pushback-reader (gen-rdr "") 5)]
    (sut/unread-char rdr \o)
    (sut/unread-char rdr \l)
    (sut/unread-char rdr \l)
    (sut/unread-char rdr \e)
    (sut/unread-char rdr \H)
    (t/is (= "Hello" (slurp rdr)))))

(t/deftest unread-chars-test
  (let [rdr (sut/pushback-reader (gen-rdr "") 5)]
    (sut/unread-chars rdr [\H \e \l \l \o])
    (t/is (= "Hello" (slurp rdr)))))
