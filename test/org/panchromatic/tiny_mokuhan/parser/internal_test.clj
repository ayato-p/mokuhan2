(ns org.panchromatic.tiny-mokuhan.parser.internal-test
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.zip :as zip]
            [org.panchromatic.tiny-mokuhan.ast2 :as ast]
            [org.panchromatic.tiny-mokuhan.parser.internal :as sut]
            [org.panchromatic.tiny-mokuhan.reader :as reader]
            [org.panchromatic.tiny-mokuhan.zip2 :as mzip]
            [org.panchromatic.tiny-mokuhan.util.string :as ustr]))

(defn- test-reader
  ([s]
   (test-reader s 1))
  ([s n]
   (reader/pushback-reader (java.io.StringReader. s) n)))

(def ^:private default-delimiters
  {:open "{{" :close "}}"})

(def ^:private initial-state
  {:ast (mzip/ast-zip)
   :template-context {:delimiters default-delimiters
                      :row 1
                      :column 1
                      :standalone? true
                      :contexts '()}})

(t/deftest lookahead-and-matched?-test
  (with-open [reader (test-reader "{{" 2)]
    (t/is (sut/lookahead-and-matched? reader "{{"))
    (t/is (= (slurp reader) "{{")))

  (with-open [reader (test-reader "{" 2)]
    (t/is (false? (sut/lookahead-and-matched? reader "{{")))
    (t/is (= (slurp reader) "{")))

  (with-open [reader (test-reader "<<<" 3)]
    (t/is (sut/lookahead-and-matched? reader "<<<"))
    (t/is (= (slurp reader) "<<<")))

  (with-open [reader (test-reader "<<||" 3)]
    (t/is (false? (sut/lookahead-and-matched? reader "<<<")))
    (t/is (= (slurp reader) "<<||"))))

(t/deftest parse-text-test
  (t/are [s expected rest-str]
      (with-open [reader (test-reader s 2)]
        (and (= {:ast (ast/syntax-tree
                       [(ast/text expected (ast/template-context default-delimiters
                                                                 1
                                                                 1
                                                                 true))])
                 :template-context {:delimiters default-delimiters
                                    :row 1
                                    :column 6
                                    :standalone? false
                                    :contexts '()}}
                (-> (sut/parse-text reader initial-state)
                    (update :ast mzip/complete)))
             (= rest-str (slurp reader))))
    "Hello" "Hello" ""
    "Hello " "Hello" " "
    "Hello\t" "Hello" "\t"
    "Hello\r\n" "Hello" "\r\n"
    "Hello\n" "Hello" "\n"
    "Hello{{" "Hello" "{{"
    "Hello{{name}}" "Hello" "{{name}}"
    "Hello world" "Hello" " world")

  (with-open [reader (test-reader "Hello{x{x" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/text "Hello{x{x" (ast/template-context default-delimiters
                                                                 1
                                                                 1
                                                                 true))])
              :template-context {:delimiters default-delimiters
                                 :row 1
                                 :column 10
                                 :standalone? false
                                 :contexts '()}}
             (-> (sut/parse-text reader initial-state)
                 (update :ast mzip/complete))))
    (t/is (= "" (slurp reader))))

  (with-open [reader (test-reader "{x{x" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/text "{x{x" (ast/template-context default-delimiters
                                                            1
                                                            1
                                                            true))])
              :template-context {:delimiters default-delimiters
                                 :row 1
                                 :column 5
                                 :standalone? false
                                 :contexts '()}}
             (-> (sut/parse-text reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "" (slurp reader)))))

(t/deftest parse-whitespace-test
  (t/are [s expected rest-str]
      (with-open [reader (test-reader s 2)]
        (and (= {:ast (ast/syntax-tree
                       [(ast/whitespace " " (ast/template-context default-delimiters
                                                                  1
                                                                  1
                                                                  true))])
                 :template-context {:delimiters default-delimiters
                                    :row 1
                                    :column 2
                                    :standalone? true
                                    :contexts '()}}
                (-> (sut/parse-whitespace reader initial-state)
                    (update :ast zip/root)))
             (= rest-str (slurp reader))))
    " " " " ""
    " x" " " "x"
    " \n" " " "\n"
    " \r\n" " " "\r\n"
    " {{foo}}" " " "{{foo}}")


  (with-open [reader (test-reader "  x" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/whitespace "  " (ast/template-context default-delimiters
                                                                1
                                                                1
                                                                true))])
              :template-context {:delimiters default-delimiters
                                 :row 1
                                 :column 3
                                 :standalone? true
                                 :contexts '()}}
             (-> (sut/parse-whitespace reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "x" (slurp reader)))))

(t/deftest parse-newline-test
  (with-open [reader (test-reader "\r\nx" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/newline "\r\n" (ast/template-context default-delimiters
                                                               1
                                                               1
                                                               true))])
              :template-context {:delimiters default-delimiters
                                 :row 2
                                 :column 1
                                 :standalone? true
                                 :contexts '()}}
             (-> (sut/parse-newline reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "x" (slurp reader))))

  (with-open [reader (test-reader "\nx" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/newline "\n" (ast/template-context default-delimiters
                                                             1
                                                             1
                                                             true))])
              :template-context {:delimiters default-delimiters
                                 :row 2
                                 :column 1
                                 :standalone? true
                                 :contexts '()}}
             (-> (sut/parse-newline reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "x" (slurp reader)))))

(t/deftest parse-variable-tag-test
  (t/testing "Successes"
    (with-open [reader (test-reader "{{foo}}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo"] (ast/template-context default-delimiters
                                                                       1
                                                                       1
                                                                       true))])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 8
                                   :standalone? false
                                   :contexts '()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{ foo }}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo"] (ast/template-context default-delimiters
                                                                       1
                                                                       1
                                                                       true))])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 10
                                   :standalone? false
                                   :contexts '()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{foo.bar}}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo" "bar"] (ast/template-context default-delimiters
                                                                             1
                                                                             1
                                                                             true))])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 12
                                   :standalone? false
                                   :contexts '()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{foo{{}}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo{{"] (ast/template-context default-delimiters
                                                                         1
                                                                         1
                                                                         true))])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 10
                                   :standalone? false
                                   :contexts '()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{fo}o}}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["fo}o"] (ast/template-context default-delimiters
                                                                        1
                                                                        1
                                                                        true))])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 9
                                   :standalone? false
                                   :contexts '()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{foo}}bar" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo"] (ast/template-context default-delimiters
                                                                       1
                                                                       1
                                                                       true))])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 8
                                   :standalone? false
                                   :contexts '()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "bar" (slurp reader)))))

  (t/testing "Errors"
    (with-open [reader (test-reader "{{foo>>")]
      (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                :cause :unclosed-tag
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{foo" 2)]
      (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                :cause :unclosed-tag
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{fo o}}" 2)]
      (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                :cause :invalid-tag-name
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{foo bar" 2)]
      (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                :cause :invalid-tag-name
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   :error))))))

(t/deftest parse-unescaped-variable-tag-test
  (t/testing "Successes"
    (with-open [reader (test-reader "{{&foo}}" 3)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/unescaped-variable-tag ["foo"] (ast/template-context default-delimiters
                                                                                 1
                                                                                 1
                                                                                 true))])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 9
                                   :standalone? false
                                   :contexts '()}}
               (-> (sut/parse-unescaped-variable-tag reader initial-state)
                   (update :ast mzip/complete)))))

    (with-open [reader (test-reader "{{& foo }}" 3)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/unescaped-variable-tag ["foo"] (ast/template-context default-delimiters
                                                                                 1
                                                                                 1
                                                                                 true))])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 11
                                   :standalone? false
                                   :contexts '()}}
               (-> (sut/parse-unescaped-variable-tag reader initial-state)
                   (update :ast mzip/complete))))))

  (t/testing "Errors"
    (with-open [reader (test-reader "{{&foo")]
      (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                :cause :unclosed-tag
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-unescaped-variable-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{&fo o")]
      (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                :cause :invalid-tag-name
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-unescaped-variable-tag reader initial-state)
                   :error))))))

(t/deftest parse-open-section-tag-test
  (t/testing "Successes"
    (with-open [r (test-reader "{{#foo}}" 3)]
      (let [state (sut/parse-open-section-tag r initial-state)]
        (t/is (= {:ast (ast/syntax-tree
                        [(ast/section
                          (ast/open-section-tag ["foo"] (ast/template-context default-delimiters
                                                                              1
                                                                              1
                                                                              true)))])
                  :template-context {:delimiters default-delimiters
                                     :row 1
                                     :column 9
                                     :standalone? false
                                     :contexts '(["foo"])}}
                 (update state :ast mzip/complete)))

        (t/is (= ::ast/section
                 (:type (zip/node (:ast state)))))

        (t/is (= "" (slurp r)))))

    (with-open [r (test-reader "{{#foo.bar}}" 3)]
      (let [state (sut/parse-open-section-tag r initial-state)]
        (t/is (= {:ast (ast/syntax-tree
                        [(ast/section
                          (ast/open-section-tag ["foo" "bar"] (ast/template-context default-delimiters
                                                                                    1
                                                                                    1
                                                                                    true)))])
                  :template-context {:delimiters default-delimiters
                                     :row 1
                                     :column 13
                                     :standalone? false
                                     :contexts '(["foo" "bar"])}}
                 (update state :ast mzip/complete)))

        (t/is (= ::ast/section
                 (:type (zip/node (:ast state)))))

        (t/is (= "" (slurp r))))))

  (t/testing "Errors"
    (with-open [reader (test-reader "{{#foo")]
      (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                :cause :unclosed-tag
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-open-section-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{#fo o")]
      (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                :cause :invalid-tag-name
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-open-section-tag reader initial-state)
                   :error))))))

(def opened-section-ast
  (let [open-section-tag-node
        (ast/open-section-tag ["foo"] (ast/template-context default-delimiters
                                                            1
                                                            1
                                                            false))]
    (-> (mzip/ast-zip)
        (mzip/append&into-section)
        (mzip/assoc-open-section-tag open-section-tag-node))))

(def opened-section-state
  {:ast opened-section-ast
   :template-context {:delimiters default-delimiters
                      :row 1
                      :column (-> opened-section-ast
                                  mzip/complete
                                  ast/syntax-tree->mustache-str
                                  ustr/length
                                  inc)
                      :standalone? false
                      :contexts '(["foo"])}})

(t/deftest parse-close-section-tag-test
  (t/testing "Successes"
    (with-open [reader (test-reader "{{/foo}}" 3)]
      (let [state (sut/parse-close-section-tag reader opened-section-state)]
        (t/is (= {:ast (ast/syntax-tree
                        [(ast/section
                          (ast/open-section-tag ["foo"] (ast/template-context default-delimiters
                                                                              1
                                                                              1
                                                                              false))
                          (ast/close-section-tag ["foo"] (ast/template-context default-delimiters
                                                                               1
                                                                               9
                                                                               false))
                          [])])
                  :template-context {:delimiters default-delimiters
                                     :row 1
                                     :column 17
                                     :standalone? false
                                     :contexts '()}}
                 (update state :ast mzip/complete))))))

  (t/testing "Errors"
    (with-open [reader (test-reader "{{/bar}}" 3)]
      (t/is (= {:error {:type :org.panchromatic.tiny-mokuhan/parse-error
                        :cause :unclosed-section
                        :occurred {:row 1
                                   :column 9
                                   :contexts '(["foo"])}}}
               (sut/parse-close-section-tag reader opened-section-state))))))

(t/deftest parse-test
  (t/testing "Successes"
    (with-open [r (test-reader "Hello, world" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= (ast/syntax-tree
                  [(ast/text "Hello," (ast/template-context default-delimiters
                                                            1
                                                            1
                                                            true))
                   (ast/whitespace " " (ast/template-context default-delimiters
                                                             1
                                                             7
                                                             false))
                   (ast/text "world" (ast/template-context default-delimiters
                                                           1
                                                           8
                                                           false))])
                 ast))

        (t/is (nil? error))))

    (with-open [r (test-reader "{Hello}, {{name}}" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= (ast/syntax-tree
                  [(ast/text "{Hello}," (ast/template-context default-delimiters
                                                              1
                                                              1
                                                              true))
                   (ast/whitespace " " (ast/template-context default-delimiters
                                                             1
                                                             9
                                                             false))
                   (ast/variable-tag ["name"] (ast/template-context default-delimiters
                                                                    1
                                                                    10
                                                                    false))])
                 ast))

        (t/is (nil? error))))

    (with-open [r (test-reader "Hello, {{name}}" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= (ast/syntax-tree
                  [(ast/text "Hello," (ast/template-context default-delimiters
                                                            1
                                                            1
                                                            true))
                   (ast/whitespace " " (ast/template-context default-delimiters
                                                             1
                                                             7
                                                             false))
                   (ast/variable-tag ["name"] (ast/template-context default-delimiters
                                                                    1
                                                                    8
                                                                    false))])
                 ast))

        (t/is (nil? error)))))

  (t/testing "Errors"
    (with-open [r (test-reader "Hello, {{name" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                  :cause :unclosed-tag
                  :occurred {:row 1 :column 8 :contexts ()}}
                 error))))

    (with-open [r (test-reader "Hello, {{&name" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= {:type :org.panchromatic.tiny-mokuhan/parse-error
                  :cause :unclosed-tag
                  :occurred {:row 1 :column 8 :contexts ()}}
                 error))))))

(t/deftest parse|standalone-test
  (with-open [reader (test-reader "{{x}}" 3)]
    (t/is (= (ast/syntax-tree
              [(ast/variable-tag ["x"] (ast/template-context default-delimiters
                                                             1
                                                             1
                                                             true))])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader " {{x}} " 3)]
    (t/is (= (ast/syntax-tree
              [(ast/whitespace " " (ast/template-context default-delimiters
                                                         1
                                                         1
                                                         false))
               (ast/variable-tag ["x"] (ast/template-context default-delimiters
                                                             1
                                                             2
                                                             true))
               (ast/whitespace " " (ast/template-context default-delimiters
                                                         1
                                                         7
                                                         false))])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader "{{x}}\n{{y}}" 3)]
    (t/is (= (ast/syntax-tree
              [(ast/variable-tag ["x"] (ast/template-context default-delimiters
                                                             1
                                                             1
                                                             true))
               (ast/newline "\n" (ast/template-context default-delimiters
                                                       1
                                                       6
                                                       false))
               (ast/variable-tag ["y"] (ast/template-context default-delimiters
                                                             2
                                                             1
                                                             true))])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader "x{{x}} " 3)]
    (t/is (= (ast/syntax-tree
              [(ast/text "x" (ast/template-context default-delimiters
                                                   1
                                                   1
                                                   false))
               (ast/variable-tag ["x"] (ast/template-context default-delimiters
                                                             1
                                                             2
                                                             false))
               (ast/whitespace " " (ast/template-context default-delimiters
                                                         1
                                                         7
                                                         false))])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader "{{foo}}{{bar}}" 3)]
    (t/is (= (ast/syntax-tree
              [(ast/variable-tag ["foo"] (ast/template-context default-delimiters
                                                               1
                                                               1
                                                               false))
               (ast/variable-tag ["bar"] (ast/template-context default-delimiters
                                                               1
                                                               8
                                                               false))])
             (-> (sut/parse reader initial-state)
                 :ast)))))
