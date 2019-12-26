(ns org.panchromatic.tiny-mokuhan.parser-test
  (:require [clojure.test :as t]
            [org.panchromatic.tiny-mokuhan.ast :as ast]
            [org.panchromatic.tiny-mokuhan.parser :as sut]))

(t/deftest text-parsing-test
  (t/testing "only text"
    (t/is (= (ast/->Mustache
              [(ast/->Text "Hello")])
             (sut/parse "Hello")))

    (t/is (= (ast/->Mustache
              [(ast/->Text "Hello")
               (ast/->NewLine)
               (ast/->Text "World")])
             (sut/parse "Hello\nWorld")))))

(t/deftest variable-parsing-test
  (t/testing "balanced"
    (t/is (= (ast/->Mustache
              [(ast/->Variable "foo" "{{" "}}")])
             (sut/parse "{{foo}}")
             (sut/parse "{{ foo }}")
             (sut/parse "{{\tfoo\t}}")
             (sut/parse "{{\nfoo\n}}")))

    (t/is (= (ast/->Mustache
              [(ast/->Text "Hello, ")
               (ast/->Variable "name" "{{" "}}")])
             (sut/parse "Hello, {{name}}"))))

  (t/testing "unbaranced"
    (t/is (= (ast/->Mustache
              [(ast/->Text "{")
               (ast/->Variable "foo" "{{" "}}")])
             (sut/parse "{{{foo}}")))

    (t/is (= (ast/->Mustache
              [(ast/->Text "{")
               (ast/->Text "{")
               (ast/->Variable "foo" "{{" "}}")])
             (sut/parse "{{{{foo}}")))

    (t/is (= (ast/->Mustache
              [(ast/->Text "{{ ")
               (ast/->Variable "foo" "{{" "}}")])
             (sut/parse "{{ {{foo}}")))))
