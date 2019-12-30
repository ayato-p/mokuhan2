(ns org.panchromatic.tiny-mokuhan.zip2-test
  (:require [org.panchromatic.tiny-mokuhan.zip2 :as mzip]
            [clojure.test :as t]
            [clojure.zip :as zip]
            [org.panchromatic.tiny-mokuhan.ast2 :as ast]))

(def ^:private dummy-tc
  (ast/template-context {:open "{{" :close "}}"} 0 0 true))

(t/deftest zip-test
  (t/is (= (ast/syntax-tree
            [(ast/text "Hello" dummy-tc)])
           (-> (mzip/ast-zip)
               (zip/append-child (ast/text "Hello" dummy-tc))
               zip/root)))

  (t/is (= (ast/syntax-tree
            [(ast/section
              (ast/open-section-tag ["foo"] dummy-tc)
              (ast/close-section-tag ["foo"] dummy-tc)
              [])])
           (-> (mzip/ast-zip)
               (zip/append-child (ast/section))
               zip/down
               (zip/edit assoc :o-tag (ast/open-section-tag ["foo"] dummy-tc))
               (zip/edit assoc :c-tag (ast/close-section-tag ["foo"] dummy-tc))
               zip/root)))

  (t/is (= (ast/syntax-tree
            [(ast/section
              (ast/open-section-tag ["foo"] dummy-tc)
              (ast/close-section-tag ["foo"] dummy-tc)
              [(ast/section
                (ast/open-section-tag ["bar"] dummy-tc)
                (ast/close-section-tag ["bar"] dummy-tc)
                [])])])
           (-> (mzip/ast-zip)
               (zip/append-child (ast/section))
               zip/down
               (zip/edit assoc :o-tag (ast/open-section-tag ["foo"] dummy-tc))
               (zip/edit assoc :c-tag (ast/close-section-tag ["foo"] dummy-tc))
               (zip/append-child (ast/section))
               zip/down
               (zip/edit assoc :o-tag (ast/open-section-tag ["bar"] dummy-tc))
               (zip/edit assoc :c-tag (ast/close-section-tag ["bar"] dummy-tc))
               zip/root))))
