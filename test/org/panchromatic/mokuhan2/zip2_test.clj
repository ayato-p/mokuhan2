(ns org.panchromatic.mokuhan2.zip2-test
  (:require [clojure.test :as t]
            [clojure.zip :as zip]
            [org.panchromatic.mokuhan2.ast2 :as ast]
            [org.panchromatic.mokuhan2.zip2 :as mzip]))

(def ^:private default-delimiters
  {:open "{{" :close "}}"})

(def ^:private dummy-tc
  {:delimiters default-delimiters
   :row 1
   :column 1
   :contexts []})

(t/deftest zip-test
  (t/is (= (ast/syntax-tree
            [(ast/text "Hello" dummy-tc)])
           (-> (mzip/ast-zip)
               (mzip/append-node (ast/text "Hello" dummy-tc))
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

(t/deftest append-node-test
  (let [text-node1 (ast/text "Hello," dummy-tc)
        ws-node (ast/whitespace " " dummy-tc)
        text-node2 (ast/text "world." dummy-tc)
        loc1 (-> (mzip/ast-zip)
                 (mzip/append-node text-node1))
        loc2 (mzip/append-node loc1 ws-node)
        loc3 (mzip/append-node loc2 text-node2)]

    (t/is (= text-node1 (zip/node loc1)))
    (t/is (= ws-node (zip/node loc2)))
    (t/is (= text-node2 (zip/node loc3)))

    (t/is (= (ast/syntax-tree
              [text-node1 ws-node text-node2])
             (mzip/complete loc3)))))

(t/deftest look-behind-for-not-standalone-test
  (t/testing "standard pattern"
    (let [tag-node1 (ast/variable-tag ["x"] (assoc dummy-tc :standalone? true))
          tag-node2 (ast/variable-tag ["y"] (assoc dummy-tc :standalone? false))]
      (t/is (= (ast/syntax-tree
                [(ast/variable-tag ["x"] (assoc dummy-tc :standalone? false))
                 (ast/variable-tag ["y"] (assoc dummy-tc :standalone? false))])
               (-> (mzip/ast-zip)
                   (mzip/append-node tag-node1)
                   (mzip/append-node tag-node2)
                   mzip/look-behind-for-not-standalone
                   mzip/complete)))))

  (t/testing "ignore whitespace"
    (let [whitespace-node (ast/whitespace "  " dummy-tc)
          tag-node1 (ast/variable-tag ["x"] (assoc dummy-tc :standalone? true))
          tag-node2 (ast/variable-tag ["y"] (assoc dummy-tc :standalone? false))]
      (t/is (= (ast/syntax-tree
                [(ast/whitespace "  " dummy-tc)
                 (ast/variable-tag ["x"] (assoc dummy-tc :standalone? false))
                 (ast/variable-tag ["y"] (assoc dummy-tc :standalone? false))])
               (-> (mzip/ast-zip)
                   (mzip/append-node whitespace-node)
                   (mzip/append-node tag-node1)
                   (mzip/append-node tag-node2)
                   mzip/look-behind-for-not-standalone
                   mzip/complete))))))
