(ns org.panchromatic.mokuhan2.parser-test
  (:require [clojure.test :as t]
            [org.panchromatic.mokuhan2.ast :as ast]
            [org.panchromatic.mokuhan2.parser :as sut]))

(t/deftest text-parsing-test
  (t/testing "only text"
    (t/is (= (ast/mustache
              [(ast/text "Hello")])
             (sut/parse "Hello")))

    (t/is (= (ast/mustache
              [(ast/text "Hello")
               (ast/newline)
               (ast/text "World")])
             (sut/parse "Hello\nWorld")))

    (t/is (= (ast/mustache
              [(ast/text "Hello ")
               (ast/newline)
               (ast/text " World")])
             (sut/parse "Hello \n World")))))

(t/deftest variable-parsing-test
  (t/testing "balanced"
    (t/is (= (ast/mustache
              [(ast/variable "foo" {:open "{{" :close "}}"})])
             (sut/parse "{{foo}}")
             (sut/parse "{{ foo }}")
             (sut/parse "{{\tfoo\t}}")
             (sut/parse "{{\nfoo\n}}")))

    (t/is (= (ast/mustache
              [(ast/text "Hello, ")
               (ast/variable "name" {:open "{{" :close "}}"})])
             (sut/parse "Hello, {{name}}"))))

  (t/testing "unbalanced"
    (t/is (= (ast/mustache
              [(ast/text "{")
               (ast/variable "foo" {:open "{{" :close "}}"})])
             (sut/parse "{{{foo}}")))

    (t/is (= (ast/mustache
              [(ast/text "{")
               (ast/text "{")
               (ast/variable "foo" {:open "{{" :close "}}"})])
             (sut/parse "{{{{foo}}")))

    (t/is (= (ast/mustache
              [(ast/variable "foo" {:open "{{" :close "}}"})
               (ast/text "}")])
             (sut/parse "{{foo}}}")))

    (t/is (= (ast/mustache
              [(ast/text "{{ ")
               (ast/variable "foo" {:open "{{" :close "}}"})])
             (sut/parse "{{ {{ foo }}")))

    (t/is (= (ast/mustache
              [(ast/variable "{{foo" {:open "{{" :close "}}"})])
             (sut/parse "{{ {{foo}}")))

    (t/is (= (ast/mustache
              [(ast/text "{{foo")])
             (sut/parse "{{foo")))))

(t/deftest unescaped-variable-parsing-test
  (t/testing "default delimiters"
    (t/testing "balanced"
      (t/is (= (ast/mustache
                [(ast/unescaped-variable "foo" {:open "{{{" :close "}}}"})])
               (sut/parse "{{{foo}}}")))

      (t/is (= (ast/mustache
                [(ast/unescaped-variable "foo" {:open "{{&" :close "}}"})])
               (sut/parse "{{&foo}}"))))

    (t/testing "unbalanced"
      (t/is (= (ast/mustache
                [(ast/unescaped-variable "{foo" {:open "{{{" :close "}}}"})])
               (sut/parse "{{{{foo}}}")))

      (t/is (= (ast/mustache
                [(ast/unescaped-variable "foo" {:open "{{{" :close "}}}"})
                 (ast/text "}")])
               (sut/parse "{{{foo}}}}")))

      (t/is (= (ast/mustache
                [(ast/text "{")
                 (ast/unescaped-variable "foo" {:open "{{&" :close "}}"})])
               (sut/parse "{{{&foo}}")))

      (t/is (= (ast/mustache
                [(ast/unescaped-variable "foo" {:open "{{&" :close "}}"})
                 (ast/text "}")])
               (sut/parse "{{&foo}}}")))))

  (t/testing "non default delimiters"
    (t/testing "balanced"
      (t/is (= (ast/mustache
                [(ast/unescaped-variable "foo" {:open "<<&" :close ">>"})])
               (sut/parse "<<&foo>>" {:delimiters {:open "<<" :close ">>"}}))))

    (t/testing "unbalanced"
      (t/is (= (ast/mustache
                [(ast/unescaped-variable "foo" {:open "<<&" :close ">>"})
                 (ast/text ">")])
               (sut/parse "<<&foo>>>" {:delimiters {:open "<<" :close ">>"}}))))

    (t/testing "triple mustache unavailable"
      (t/is (= (ast/mustache
                [(ast/text "{{{foo}}}")])
               (sut/parse "{{{foo}}}" {:delimiters {:open "||" :close "||"}})))

      (t/is (= (ast/mustache
                [(ast/text "<<{foo}}}")])
               (sut/parse "<<{foo}}}" {:delimiters {:open "<<" :close "}}}"}}))))))

(t/deftest default-delimiters-test
  (t/is (= (ast/mustache
            [(ast/variable "foo" {:open "{{" :close "}}"})])
           (sut/parse "{{foo}}")
           (sut/parse "{{foo}}" {:delimiters {:open "{{" :close "}}"}})))

  (let [options {:delimiters {:open "<<" :close ">>"}}]
    (t/is (= (ast/mustache
              [(ast/variable "foo" {:open "<<" :close ">>"})])
             (sut/parse "<<foo>>" options)
             (sut/parse "<< foo >>" options)))

    (t/is (= (ast/mustache
              [(ast/text "{{foo}}")])
             (sut/parse "{{foo}}" options)))

    (t/is (= (ast/mustache
              [(ast/text "<< ")
               (ast/variable "foo" {:open "<<" :close ">>"})])
             (sut/parse "<< << foo >>" options)))))
