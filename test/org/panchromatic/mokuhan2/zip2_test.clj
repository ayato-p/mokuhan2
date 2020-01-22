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
              (ast/section-open-tag ["foo"] dummy-tc)
              (ast/section-close-tag ["foo"] dummy-tc)
              [])])
           (-> (mzip/ast-zip)
               (mzip/append-node (ast/section))
               zip/down
               (mzip/append-node (ast/section-open-tag ["foo"] dummy-tc))
               (mzip/append-node (ast/section-close-tag ["foo"] dummy-tc))
               zip/up
               (zip/edit assoc :closed? true)
               zip/root)))

  (t/is (= (ast/syntax-tree
            [(ast/section
              (ast/section-open-tag ["foo"] dummy-tc)
              (ast/section-close-tag ["foo"] dummy-tc)
              [(ast/section
                (ast/section-open-tag ["bar"] dummy-tc)
                (ast/section-close-tag ["bar"] dummy-tc)
                [])])])
           (-> (mzip/ast-zip)
               (mzip/append-node (ast/section))
               zip/down
               (mzip/append-node (ast/section-open-tag ["foo"] dummy-tc))
               (mzip/append-node (ast/section))
               zip/down
               (mzip/append-node (ast/section-open-tag ["bar"] dummy-tc))
               (mzip/append-node (ast/section-close-tag ["bar"] dummy-tc))
               zip/up
               (zip/edit assoc :closed? true)
               (mzip/append-node (ast/section-close-tag ["foo"] dummy-tc))
               zip/up
               (zip/edit assoc :closed? true)
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
                   mzip/look-behind
                   (mzip/append-node tag-node2)
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
                   mzip/look-behind
                   (mzip/append-node tag-node2)
                   mzip/complete)))

      (t/is (= (ast/syntax-tree
                [(ast/variable-tag ["x"] (assoc dummy-tc :standalone? false))
                 (ast/whitespace "  " dummy-tc)
                 (ast/variable-tag ["y"] (assoc dummy-tc :standalone? false))])
               (-> (mzip/ast-zip)
                   (mzip/append-node tag-node1)
                   (mzip/append-node whitespace-node)
                   mzip/look-behind
                   (mzip/append-node tag-node2)
                   mzip/complete)))))

  (t/testing "with section"
    (t/is (= (ast/syntax-tree
              [(ast/variable-tag ["y"] (assoc dummy-tc :standalone? false))
               (ast/section
                (ast/section-open-tag ["x"] (assoc dummy-tc :standalone? false)))])
             (-> (mzip/ast-zip)
                 (mzip/append-node (ast/variable-tag ["y"] dummy-tc))
                 mzip/look-behind
                 (mzip/open-section (ast/section-open-tag ["x"] (assoc dummy-tc :standalone? false)))
                 mzip/complete)))

    (t/is (= (ast/syntax-tree
              [(ast/section
                (ast/section-open-tag ["x"] (assoc dummy-tc :standalone? false))
                nil
                [(ast/variable-tag ["y"] (assoc dummy-tc :standalone? false))])])
             (-> (mzip/ast-zip)
                 (mzip/open-section (ast/section-open-tag ["x"] (assoc dummy-tc :standalone? true)))
                 mzip/look-behind
                 (mzip/append-node (ast/variable-tag ["y"] (assoc dummy-tc :standalone? false)))
                 mzip/complete)))

    (t/is (= (ast/syntax-tree
              [(ast/section
                (ast/section-open-tag ["x"] (assoc dummy-tc :standalone? true))
                (ast/section-close-tag ["x"] (assoc dummy-tc :standalone? false))
                [(ast/newline "\r\n" dummy-tc)])
               (ast/variable-tag ["y"] (assoc dummy-tc :standalone? false))])
             (-> (mzip/ast-zip)
                 (mzip/open-section (ast/section-open-tag ["x"] (assoc dummy-tc :standalone? true)))
                 (mzip/append-node (ast/newline "\r\n" dummy-tc))

                 (mzip/close-section (ast/section-close-tag ["x"] (assoc dummy-tc :standalone? true)))
                 mzip/look-behind
                 (mzip/append-node (ast/variable-tag ["y"] (assoc dummy-tc :standalone? false)))
                 mzip/complete)))))

(t/deftest open-section-test
  (t/testing "section should be opend"
    (t/is (= (ast/syntax-tree
              [(ast/section
                (ast/section-open-tag ["x"] dummy-tc))])
             (-> (mzip/ast-zip)
                 (mzip/open-section (ast/section-open-tag ["x"] dummy-tc))
                 mzip/complete)))))

(t/deftest close-section-test
  (t/testing "section should be closed"
    (t/is (= (ast/syntax-tree
              [(ast/section
                (ast/section-open-tag ["x"] dummy-tc)
                (ast/section-close-tag ["x"] dummy-tc))])
             (-> (mzip/ast-zip)
                 (mzip/open-section (ast/section-open-tag ["x"] dummy-tc))
                 (mzip/close-section (ast/section-close-tag ["x"] dummy-tc))
                 mzip/complete)))))
