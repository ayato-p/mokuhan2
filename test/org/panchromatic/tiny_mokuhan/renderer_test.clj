(ns org.panchromatic.tiny-mokuhan.renderer-test
  (:require [org.panchromatic.tiny-mokuhan.renderer :as sut]
            [clojure.test :as t]
            [org.panchromatic.tiny-mokuhan.ast :as ast]))

(t/deftest render-text-test
  (let [ast (ast/mustache
             {:contents
              [(ast/text {:content "Hello, world"})]})]
    (t/is (= "Hello, world"
             (sut/render ast))))

  (let [ast (ast/mustache
             {:contents
              [(ast/text {:content "Hello"})
               (ast/newline)
               (ast/text {:content "world"})]})]
    (t/is (= "Hello\nworld"
             (sut/render ast)))))
