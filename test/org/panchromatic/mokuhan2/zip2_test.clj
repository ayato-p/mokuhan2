(ns org.panchromatic.mokuhan2.zip2-test
  (:require [org.panchromatic.mokuhan2.zip2 :as mzip]
            [clojure.test :as t]
            [clojure.zip :as zip]
            [org.panchromatic.mokuhan2.ast2 :as ast]))

(def ^:private default-delimiters
  {:open "{{" :close "}}"})

(def ^:private dummy-tc
  (ast/template-context {:open "{{" :close "}}"} 0 0 true))

(t/deftest zip-test
  (t/is (= (ast/syntax-tree
            [(ast/text "Hello" dummy-tc)])
           (-> (mzip/ast-zip)
               (mzip/append-primitive (ast/text "Hello" dummy-tc))
               mzip/complete)))

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

(t/deftest append-tag-test
  (t/is (= (ast/syntax-tree
            [(ast/variable-tag ["x"] (ast/template-context default-delimiters 1 1 true))])

           (let [v1 (ast/variable-tag ["x"] (ast/template-context default-delimiters
                                                                  1
                                                                  1
                                                                  true))]
             (-> (mzip/ast-zip)
                 (mzip/append-tag v1)
                 mzip/complete))))

  (t/is (= (ast/syntax-tree
            [(ast/variable-tag ["x"] (ast/template-context default-delimiters 1 1 false))
             (ast/variable-tag ["y"] (ast/template-context default-delimiters 1 6 false))])
           (let [v1 (ast/variable-tag ["x"] (ast/template-context default-delimiters
                                                                  1
                                                                  1
                                                                  true))
                 v2 (ast/variable-tag ["y"] (ast/template-context default-delimiters
                                                                  1
                                                                  6
                                                                  false))]
             (-> (mzip/ast-zip)
                 (mzip/append-tag v1)
                 (mzip/append-tag v2)
                 mzip/complete))))

  (t/is (= (ast/syntax-tree
            [(ast/variable-tag ["x"] (ast/template-context default-delimiters 1 1 true))
             (ast/newline "\n" (ast/template-context default-delimiters 1 6 false))
             (ast/variable-tag ["y"] (ast/template-context default-delimiters 2 1 true))])
           (let [v1 (ast/variable-tag ["x"] (ast/template-context default-delimiters
                                                                  1
                                                                  1
                                                                  true))
                 nl (ast/newline "\n" (ast/template-context default-delimiters 1 6 false))
                 v2 (ast/variable-tag ["y"] (ast/template-context default-delimiters
                                                                  2
                                                                  1
                                                                  true))]
             (-> (mzip/ast-zip)
                 (mzip/append-tag v1)
                 (mzip/append-primitive nl)
                 (mzip/append-tag v2)
                 mzip/complete)))))

(t/deftest append&into-section-test
  (let [loc (-> (mzip/ast-zip)
                mzip/append&into-section)]
    (t/is (= (ast/syntax-tree
              [(ast/section)])
             (mzip/complete loc)))

    (t/is (= ::ast/section
             (:type (zip/node loc))))))

(t/deftest out-section-test
  (t/is (= ::ast/syntax-tree
           (-> (mzip/ast-zip)
               mzip/append&into-sections
               mzip/out-section
               zip/node
               :type))))

(t/deftest assoc-open-section-tag-test
  (t/is (= (ast/syntax-tree
            [(ast/section
              (ast/open-section-tag ["x"] (ast/template-context default-delimiters
                                                                1
                                                                1
                                                                true)))])
           (let [open-tag (ast/open-section-tag ["x"] (ast/template-context default-delimiters
                                                                            1
                                                                            1
                                                                            true))]
             (-> (mzip/ast-zip)
                 mzip/append&into-section
                 (mzip/assoc-open-section-tag open-tag)
                 mzip/complete)))))