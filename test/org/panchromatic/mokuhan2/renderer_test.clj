(ns org.panchromatic.mokuhan2.renderer-test
  (:require [org.panchromatic.mokuhan2.renderer :as sut]
            [clojure.test :as t]
            [org.panchromatic.mokuhan2.ast :as ast]))

(t/deftest render-text-test
  (let [ast (ast/mustache
             [(ast/text "Hello, world")])]
    (t/is (= "Hello, world" (sut/render ast {}))))

  (let [ast (ast/mustache
             [(ast/text "Hello")
              (ast/newline)
              (ast/text "world")])]
    (t/is (= "Hello\nworld" (sut/render ast {})))))

(t/deftest render-variable-test
  (t/testing "simple interpolation"
    (let [ast (ast/mustache
               [(ast/text "Hello, ")
                (ast/variable "name" {:open "{{" :close "}}"})
                (ast/text ".")])]
      (t/is (= "Hello, Taro."
               (sut/render ast {:name "Taro"})))))

  (t/testing "should escape"
    (let [ast (ast/mustache
               [(ast/variable "x" {:open "{{" :close "}}"})])]
      (t/is (= "&lt;&#39;name&#39;&gt;"
               (sut/render ast {:x "<'name'>"}))))))
