(ns org.panchromatic.mokuhan2.parser.internal-test
  (:require [clojure.test :as t]
            [clojure.zip :as zip]
            [org.panchromatic.mokuhan2.ast2 :as ast]
            [org.panchromatic.mokuhan2.parser.internal :as sut]
            [org.panchromatic.mokuhan2.reader :as reader]
            [org.panchromatic.mokuhan2.util.string :as ustr]
            [org.panchromatic.mokuhan2.zip2 :as mzip]))

(defn- test-reader
  ([s]
   (test-reader s 3))
  ([s n]
   (reader/pushback-reader (java.io.StringReader. s) n)))

(def ^:private default-delimiters
  {:open "{{" :close "}}"})

(def ^:private initial-state
  {:ast              (mzip/ast-zip)
   :template-context {:delimiters  default-delimiters
                      :row         1
                      :column      1
                      :contexts    []
                      :line-nodes []}})

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
        (t/is (= {:ast (ast/syntax-tree
                        [(ast/text expected {:delimiters default-delimiters
                                             :row 1
                                             :column 1
                                             :contexts []})])
                  :template-context {:delimiters default-delimiters
                                     :row 1
                                     :column 6
                                     :contexts []
                                     :line-nodes [::ast/text]}}
                 (-> (sut/parse-text reader initial-state)
                     (update :ast mzip/complete))))
        (t/is (= rest-str (slurp reader))))
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
                    [(ast/text "Hello{x{x" {:delimiters default-delimiters
                                            :row 1
                                            :column 1
                                            :contexts []})])
              :template-context {:delimiters default-delimiters
                                 :row 1
                                 :column 10
                                 :contexts []
                                 :line-nodes [::ast/text]}}
             (-> (sut/parse-text reader initial-state)
                 (update :ast mzip/complete))))
    (t/is (= "" (slurp reader))))

  (with-open [reader (test-reader "{x{x" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/text "{x{x" {:delimiters default-delimiters
                                       :row 1
                                       :column 1
                                       :contexts []})])
              :template-context {:delimiters default-delimiters
                                 :row 1
                                 :column 5
                                 :contexts []
                                 :line-nodes [::ast/text]}}
             (-> (sut/parse-text reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "" (slurp reader)))))

(t/deftest parse-whitespace-test
  (t/are [s expected rest-str]
      (with-open [reader (test-reader s 2)]
        (t/is (= {:ast (ast/syntax-tree
                        [(ast/whitespace " " {:delimiters default-delimiters
                                              :row 1
                                              :column 1
                                              :contexts []})])
                  :template-context {:delimiters default-delimiters
                                     :row 1
                                     :column 2
                                     :contexts []
                                     :line-nodes [::ast/whitespace]}}
                 (-> (sut/parse-whitespace reader initial-state)
                     (update :ast zip/root))))

        (t/is (= rest-str (slurp reader))))
    " " " " ""
    " x" " " "x"
    " \n" " " "\n"
    " \r\n" " " "\r\n"
    " {{foo}}" " " "{{foo}}")


  (with-open [reader (test-reader "  x" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/whitespace "  " {:delimiters default-delimiters
                                           :row 1
                                           :column 1
                                           :contexts []})])
              :template-context {:delimiters default-delimiters
                                 :row 1
                                 :column 3
                                 :contexts []
                                 :line-nodes [::ast/whitespace]}}
             (-> (sut/parse-whitespace reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "x" (slurp reader)))))

(t/deftest parse-newline-test
  (with-open [reader (test-reader "\r\nx" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/newline "\r\n" {:delimiters default-delimiters
                                          :row 1
                                          :column 1
                                          :contexts []})])
              :template-context {:delimiters default-delimiters
                                 :row 2
                                 :column 1
                                 :contexts []
                                 :line-nodes []}}
             (-> (sut/parse-newline reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "x" (slurp reader))))

  (with-open [reader (test-reader "\nx" 2)]
    (t/is (= {:ast (ast/syntax-tree
                    [(ast/newline "\n" {:delimiters default-delimiters
                                        :row 1
                                        :column 1
                                        :contexts []})])
              :template-context {:delimiters default-delimiters
                                 :row 2
                                 :column 1
                                 :contexts []
                                 :line-nodes []}}
             (-> (sut/parse-newline reader initial-state)
                 (update :ast zip/root))))
    (t/is (= "x" (slurp reader)))))

(t/deftest parse-variable-tag-test
  (t/testing "Successes"
    (with-open [reader (test-reader "{{foo}}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo"] {:delimiters default-delimiters
                                                  :row 1
                                                  :column 1
                                                  :standalone? true
                                                  :contexts []})])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 8
                                   :contexts []
                                   :line-nodes [::ast/variable-tag]}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{ foo }}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo"] {:delimiters default-delimiters
                                                  :row 1
                                                  :column 1
                                                  :standalone? true
                                                  :contexts []})])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 10
                                   :contexts []
                                   :line-nodes [::ast/variable-tag]}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{foo.bar}}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo" "bar"] {:delimiters default-delimiters
                                                        :row 1
                                                        :column 1
                                                        :standalone? true
                                                        :contexts []})])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 12
                                   :contexts []
                                   :line-nodes [::ast/variable-tag]}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{foo{{}}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo{{"] {:delimiters default-delimiters
                                                    :row 1
                                                    :column 1
                                                    :standalone? true
                                                    :contexts []})])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 10
                                   :contexts []
                                   :line-nodes [::ast/variable-tag]}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{fo}o}}" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["fo}o"] {:delimiters default-delimiters
                                                   :row 1
                                                   :column 1
                                                   :standalone? true
                                                   :contexts []})])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 9
                                   :contexts []
                                   :line-nodes [::ast/variable-tag]}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "" (slurp reader))))

    (with-open [reader (test-reader "{{foo}}bar" 2)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/variable-tag ["foo"] {:delimiters default-delimiters
                                                  :row 1
                                                  :column 1
                                                  :standalone? true
                                                  :contexts []})])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 8
                                   :contexts []
                                   :line-nodes [::ast/variable-tag]}}
               (-> (sut/parse-variable-tag reader initial-state)
                   (update :ast mzip/complete))))

      (t/is (= "bar" (slurp reader)))))

  (t/testing "Errors"
    (with-open [reader (test-reader "{{foo>>")]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :unclosed-tag
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{foo" 2)]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :unclosed-tag
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{fo o}}" 2)]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :invalid-tag-name
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{foo bar" 2)]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :invalid-tag-name
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{f\noo}}")]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :invalid-tag-name
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-variable-tag reader initial-state)
                   :error))))))

(t/deftest parse-unescaped-variable-tag-test
  (t/testing "Successes"
    (with-open [reader (test-reader "{{&foo}}" 3)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/unescaped-variable-tag ["foo"] {:delimiters default-delimiters
                                                            :row 1
                                                            :column 1
                                                            :standalone? true
                                                            :contexts []})])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 9
                                   :contexts []
                                   :line-nodes [::ast/unescaped-variable-tag]}}
               (-> (sut/parse-unescaped-variable-tag reader initial-state)
                   (update :ast mzip/complete)))))

    (with-open [reader (test-reader "{{& foo }}" 3)]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/unescaped-variable-tag ["foo"] {:delimiters default-delimiters
                                                            :row 1
                                                            :column 1
                                                            :standalone? true
                                                            :contexts []})])
                :template-context {:delimiters default-delimiters
                                   :row 1
                                   :column 11
                                   :contexts []
                                   :line-nodes [::ast/unescaped-variable-tag]}}
               (-> (sut/parse-unescaped-variable-tag reader initial-state)
                   (update :ast mzip/complete))))))

  (t/testing "Errors"
    (with-open [reader (test-reader "{{&foo")]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :unclosed-tag
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-unescaped-variable-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{&fo o")]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :invalid-tag-name
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-unescaped-variable-tag reader initial-state)
                   :error))))))

(t/deftest parse-section-open-tag-test
  (t/testing "Successes"
    (with-open [r (test-reader "{{#foo}}" 3)]
      (let [state (sut/parse-section-open-tag r initial-state)]
        (t/is (= {:ast (ast/syntax-tree
                        [(ast/section
                          (ast/section-open-tag ["foo"] {:delimiters default-delimiters
                                                         :row 1
                                                         :column 1
                                                         :standalone? true
                                                         :contexts []}))])
                  :template-context {:delimiters default-delimiters
                                     :row 1
                                     :column 9
                                     :line-nodes [::ast/section-open-tag]
                                     :contexts [["foo"]]}}
                 (update state :ast mzip/complete)))

        (t/is (= ::ast/section-open-tag
                 (:type (zip/node (:ast state)))))

        (t/is (= "" (slurp r)))))

    (with-open [r (test-reader "{{#foo.bar}}" 3)]
      (let [state (sut/parse-section-open-tag r initial-state)]
        (t/is (= {:ast (ast/syntax-tree
                        [(ast/section
                          (ast/section-open-tag ["foo" "bar"] {:delimiters default-delimiters
                                                               :row 1
                                                               :column 1
                                                               :standalone? true
                                                               :contexts []}))])
                  :template-context {:delimiters default-delimiters
                                     :row 1
                                     :column 13
                                     :line-nodes [::ast/section-open-tag]
                                     :contexts [["foo" "bar"]]}}
                 (update state :ast mzip/complete)))

        (t/is (= ::ast/section-open-tag
                 (:type (zip/node (:ast state)))))

        (t/is (= "" (slurp r))))))

  (t/testing "Errors"
    (with-open [reader (test-reader "{{#foo")]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :unclosed-tag
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-section-open-tag reader initial-state)
                   :error))))

    (with-open [reader (test-reader "{{#fo o")]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :invalid-tag-name
                :occurred {:row 1 :column 1 :contexts ()}}
               (-> (sut/parse-section-open-tag reader initial-state)
                   :error))))))

(def opened-section-ast
  (let [open-section-tag-node
        (ast/section-open-tag ["foo"] {:delimiters default-delimiters
                                       :row 1
                                       :column 1
                                       :standalone? true
                                       :contexts []})]
    (-> (mzip/ast-zip)
        (mzip/open-section open-section-tag-node))))

(def opened-section-state
  {:ast opened-section-ast
   :template-context {:delimiters default-delimiters
                      :row 1
                      :column (-> opened-section-ast
                                  mzip/complete
                                  ast/syntax-tree->mustache-str
                                  ustr/length
                                  inc)
                      :line-nodes [::ast/section-open-tag]
                      :contexts '(["foo"])}})

(t/deftest parse-section-close-tag-test
  (t/testing "Successes"
    (with-open [reader (test-reader "{{/foo}}" 3)]
      (let [state (sut/parse-section-close-tag reader opened-section-state)]
        (t/is (= {:ast (ast/syntax-tree
                        [(ast/section
                          (ast/section-open-tag ["foo"] {:delimiters default-delimiters
                                                         :row 1
                                                         :column 1
                                                         :standalone? false
                                                         :contexts []})
                          (ast/section-close-tag ["foo"] {:delimiters default-delimiters
                                                          :row 1
                                                          :column 9
                                                          :standalone? false
                                                          :contexts []})
                          [])])
                  :template-context {:delimiters default-delimiters
                                     :row 1
                                     :column 17
                                     :line-nodes [::ast/section-open-tag ::ast/section-close-tag]
                                     :contexts []}}
                 (update state :ast mzip/complete))))))

  (t/testing "Errors"
    (with-open [reader (test-reader "{{/bar}}" 3)]
      (t/is (= {:error {:type :org.panchromatic.mokuhan2/parse-error
                        :cause :unclosed-section
                        :occurred {:row 1
                                   :column 9
                                   :contexts '(["foo"])}}}
               (sut/parse-section-close-tag reader opened-section-state))))

    (with-open [reader (test-reader "{{/foo")]
      (t/is (= {:error {:type :org.panchromatic.mokuhan2/parse-error
                        :cause :unclosed-tag
                        :occurred {:row 1
                                   :column 9
                                   :contexts '(["foo"])}}}
               (sut/parse-section-close-tag reader opened-section-state))))

    (with-open [reader (test-reader "{{/fo o}}")]
      (t/is (= {:error {:type :org.panchromatic.mokuhan2/parse-error
                        :cause :invalid-tag-name
                        :occurred {:row 1
                                   :column 9
                                   :contexts '(["foo"])}}}
               (sut/parse-section-close-tag reader opened-section-state))))))

(t/deftest parse-section-test
  (t/testing "Successes"
    (t/testing "nested sections"
      (with-open [r (test-reader "{{#x}}{{#y}}ok{{/y}}{{/x}}" 3)]
        (t/is (= (ast/syntax-tree
                  [(ast/section
                    (ast/section-open-tag ["x"] {:delimiters default-delimiters
                                                 :row 1 :column 1
                                                 :standalone? false
                                                 :contexts []})
                    (ast/section-close-tag ["x"] {:delimiters default-delimiters
                                                  :row 1 :column 21
                                                  :standalone? false
                                                  :contexts []})
                    [(ast/section
                      (ast/section-open-tag ["y"] {:delimiters default-delimiters
                                                   :row 1 :column 7
                                                   :standalone? false
                                                   :contexts [["x"]]})
                      (ast/section-close-tag ["y"] {:delimiters default-delimiters
                                                    :row 1 :column 15
                                                    :standalone? false
                                                    :contexts [["x"]]})
                      [(ast/text "ok" {:delimiters default-delimiters
                                       :row 1 :column 13
                                       :contexts [["x"] ["y"]]})])])])
                 (:ast (sut/parse r initial-state)))))

      (with-open [r (test-reader (str "{{#x}}\n"
                                      "{{#y}}ok{{/y}}\n"
                                      "{{/x}}"))]
        (t/is (= (ast/syntax-tree
                  [(ast/section
                    (ast/section-open-tag ["x"] {:delimiters default-delimiters
                                                 :row 1 :column 1
                                                 :standalone? true
                                                 :contexts []})
                    (ast/section-close-tag ["x"] {:delimiters default-delimiters
                                                  :row 3 :column 1
                                                  :standalone? true
                                                  :contexts []})
                    [(ast/newline "\n" {:delimiters default-delimiters
                                        :row 1 :column 7
                                        :contexts [["x"]]})
                     (ast/section
                      (ast/section-open-tag ["y"] {:delimiters default-delimiters
                                                   :row 2 :column 1
                                                   :standalone? false
                                                   :contexts [["x"]]})
                      (ast/section-close-tag ["y"] {:delimiters default-delimiters
                                                    :row 2 :column 9
                                                    :standalone? false
                                                    :contexts [["x"]]})
                      [(ast/text "ok" {:delimiters default-delimiters
                                       :row 2 :column 7
                                       :contexts [["x"] ["y"]]})])
                     (ast/newline "\n" {:delimiters default-delimiters
                                        :row 2 :column 15
                                        :contexts [["x"]]})])])
                 (:ast (sut/parse r initial-state)))))

      (with-open [r (test-reader (str "{{#x}}\n"
                                      "{{#y}}\n"
                                      "ok\n"
                                      "{{/y}}\n"
                                      "{{/x}}"))]
        (t/is (= (ast/syntax-tree
                  [(ast/section
                    (ast/section-open-tag ["x"] {:delimiters default-delimiters
                                                 :row 1 :column 1
                                                 :standalone? true
                                                 :contexts []})
                    (ast/section-close-tag ["x"] {:delimiters default-delimiters
                                                  :row 5 :column 1
                                                  :standalone? true
                                                  :contexts []})
                    [(ast/newline "\n" {:delimiters default-delimiters
                                        :row 1 :column 7
                                        :contexts [["x"]]})
                     (ast/section
                      (ast/section-open-tag ["y"] {:delimiters default-delimiters
                                                   :row 2 :column 1
                                                   :standalone? true
                                                   :contexts [["x"]]})
                      (ast/section-close-tag ["y"] {:delimiters default-delimiters
                                                    :row 4 :column 1
                                                    :standalone? true
                                                    :contexts [["x"]]})
                      [(ast/newline "\n" {:delimiters default-delimiters
                                          :row 2 :column 7
                                          :contexts [["x"] ["y"]]})
                       (ast/text "ok" {:delimiters default-delimiters
                                       :row 3 :column 1
                                       :contexts [["x"] ["y"]]})
                       (ast/newline "\n" {:delimiters default-delimiters
                                          :row 3 :column 3
                                          :contexts [["x"] ["y"]]})])
                     (ast/newline "\n" {:delimiters default-delimiters
                                        :row 4 :column 7
                                        :contexts [["x"]]})])])
                 (:ast (sut/parse r initial-state))))))

    (t/testing "dotted name sections"
      (with-open [r (test-reader "{{#x.y}}{{#a.b}}ok{{/a.b}}{{/x.y}}")]
        (t/is (= (ast/syntax-tree
                  [(ast/section
                    (ast/section-open-tag ["x" "y"] {:delimiters default-delimiters
                                                     :row 1 :column 1
                                                     :standalone? false
                                                     :contexts []})
                    (ast/section-close-tag ["x" "y"] {:delimiters default-delimiters
                                                      :row 1 :column 27
                                                      :standalone? false
                                                      :contexts []})
                    [(ast/section
                      (ast/section-open-tag ["a" "b"] {:delimiters default-delimiters
                                                       :row 1 :column 9
                                                       :standalone? false
                                                       :contexts [["x" "y"]]})
                      (ast/section-close-tag ["a" "b"] {:delimiters default-delimiters
                                                        :row 1 :column 19
                                                        :standalone? false
                                                        :contexts [["x" "y"]]})
                      [(ast/text "ok" {:delimiters default-delimiters
                                       :row 1 :column 17
                                       :contexts [["x" "y"] ["a" "b"]]})])])])
                 (:ast (sut/parse r initial-state)))))))

  (t/testing "Errors"
    (with-open [r (test-reader "{{#x}}{{#y}}{{/x}}{{/y}}")]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :unclosed-section
                :occurred {:row 1 :column 13 :contexts [["x"] ["y"]]}}
               (:error (sut/parse r initial-state)))))

    (with-open [r (test-reader "{{#x}}{{#y}}{{#z}}{{/z}}{{/x}}{{/y}}")]
      (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                :cause :unclosed-section
                :occurred {:row 1 :column 25 :contexts [["x"] ["y"]]}}
               (:error (sut/parse r initial-state)))))))

(t/deftest parse-inverted-section-open-tag-test
  (t/testing "Successes"
    (with-open [r (test-reader "{{^x}}")]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/inverted-section
                        (ast/inverted-section-open-tag ["x"] {:delimiters default-delimiters
                                                              :row 1 :column 1
                                                              :standalone? true
                                                              :contexts []}))])
                :template-context {:delimiters default-delimiters
                                   :row 1 :column 7
                                   :line-nodes [::ast/inverted-section-open-tag]
                                   :contexts [["x"]]}}
               (-> (sut/parse-inverted-section-open-tag r initial-state)
                   (update :ast mzip/complete)))))

    (with-open [r (test-reader "{{^ x }}")]
      (t/is (= {:ast (ast/syntax-tree
                      [(ast/inverted-section
                        (ast/inverted-section-open-tag ["x"] {:delimiters default-delimiters
                                                              :row 1 :column 1
                                                              :standalone? true
                                                              :contexts []}))])
                :template-context {:delimiters default-delimiters
                                   :row 1 :column 9
                                   :line-nodes [::ast/inverted-section-open-tag]
                                   :contexts [["x"]]}}
               (-> (sut/parse-inverted-section-open-tag r initial-state)
                   (update :ast mzip/complete)))))))

(t/deftest parse-test
  (t/testing "Successes"
    (with-open [r (test-reader "Hello, world" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= (ast/syntax-tree
                  [(ast/text "Hello," {:delimiters default-delimiters
                                       :row 1
                                       :column 1
                                       :contexts []})
                   (ast/whitespace " " {:delimiters default-delimiters
                                        :row 1
                                        :column 7
                                        :contexts []})
                   (ast/text "world" {:delimiters default-delimiters
                                      :row 1
                                      :column 8
                                      :contexts []})])
                 ast))

        (t/is (nil? error))))

    (with-open [r (test-reader "{Hello}, {{name}}" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= (ast/syntax-tree
                  [(ast/text "{Hello}," {:delimiters default-delimiters
                                         :row 1
                                         :column 1
                                         :contexts []})
                   (ast/whitespace " " {:delimiters default-delimiters
                                        :row 1
                                        :column 9
                                        :contexts []})
                   (ast/variable-tag ["name"] {:delimiters default-delimiters
                                               :row 1
                                               :column 10
                                               :standalone? false
                                               :contexts []})])
                 ast))

        (t/is (nil? error))))

    (with-open [r (test-reader "Hello, {{name}}" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= (ast/syntax-tree
                  [(ast/text "Hello," {:delimiters default-delimiters
                                       :row 1
                                       :column 1
                                       :contexts []})
                   (ast/whitespace " " {:delimiters default-delimiters
                                        :row 1
                                        :column 7
                                        :contexts []})
                   (ast/variable-tag ["name"] {:delimiters default-delimiters
                                               :row 1
                                               :column 8
                                               :standalone? false
                                               :contexts []})])
                 ast))

        (t/is (nil? error))))

    (with-open [r (test-reader "{{^x}}{{x}}{{/x}}")]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= (ast/syntax-tree
                  [(ast/inverted-section
                    (ast/inverted-section-open-tag ["x"] {:delimiters default-delimiters
                                                          :row 1 :column 1
                                                          :standalone? false
                                                          :contexts []})
                    (ast/section-close-tag ["x"] {:delimiters default-delimiters
                                                  :row 1 :column 12
                                                  :standalone? false
                                                  :contexts []})
                    [(ast/variable-tag ["x"] {:delimiters default-delimiters
                                              :row 1 :column 7
                                              :standalone? false
                                              :contexts [["x"]]})])])
                 ast))))

    (with-open [r (test-reader (str "{{# x }}\n"
                                    " {{#y}} {{ y }} {{/y}} \n"
                                    "{{/ x }}\n") 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= (ast/syntax-tree
                  [(ast/section
                    (ast/section-open-tag ["x"] {:delimiters default-delimiters
                                                 :row 1
                                                 :column 1
                                                 :standalone? true
                                                 :contexts []})
                    (ast/section-close-tag ["x"] {:delimiters default-delimiters
                                                  :row 3
                                                  :column 1
                                                  :standalone? true
                                                  :contexts []})
                    [(ast/newline "\n" {:delimiters default-delimiters
                                        :row 1
                                        :column 9
                                        :contexts [["x"]]})
                     (ast/whitespace " " {:delimiters default-delimiters
                                          :row 2
                                          :column 1
                                          :contexts [["x"]]})
                     (ast/section
                      (ast/section-open-tag ["y"] {:delimiters default-delimiters
                                                   :row 2
                                                   :column 2
                                                   :standalone? false
                                                   :contexts [["x"]]})
                      (ast/section-close-tag ["y"] {:delimiters default-delimiters
                                                    :row 2
                                                    :column 17
                                                    :standalone? false
                                                    :contexts [["x"]]})
                      [(ast/whitespace " " {:delimiters default-delimiters
                                            :row 2
                                            :column 8
                                            :contexts [["x"]  ["y"]]})
                       (ast/variable-tag ["y"] {:delimiters default-delimiters
                                                :row 2
                                                :column 9
                                                :standalone? false
                                                :contexts [["x"] ["y"]]})
                       (ast/whitespace " " {:delimiters default-delimiters
                                            :row 2
                                            :column 16
                                            :contexts [["x"]  ["y"]]})])
                     (ast/whitespace " " {:delimiters default-delimiters
                                          :row 2
                                          :column 23
                                          :contexts [["x"]]})
                     (ast/newline "\n" {:delimiters default-delimiters
                                        :row 2
                                        :column 24
                                        :contexts [["x"]]})])
                   (ast/newline "\n" {:delimiters default-delimiters
                                      :row 3
                                      :column 9
                                      :contexts []})])
                 ast)))))

  (t/testing "Errors"
    (with-open [r (test-reader "Hello, {{name" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                  :cause :unclosed-tag
                  :occurred {:row 1 :column 8 :contexts ()}}
                 error))))

    (with-open [r (test-reader "Hello, {{&name" 3)]
      (let [{:keys [ast error]} (sut/parse r initial-state)]
        (t/is (= {:type :org.panchromatic.mokuhan2/parse-error
                  :cause :unclosed-tag
                  :occurred {:row 1 :column 8 :contexts ()}}
                 error))))))

(t/deftest parse|standalone-test
  (with-open [reader (test-reader "{{x}}" 3)]
    (t/is (= (ast/syntax-tree
              [(ast/variable-tag ["x"] {:delimiters default-delimiters
                                        :row 1
                                        :column 1
                                        :standalone? true
                                        :contexts []})])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader " {{x}} " 3)]
    (t/is (= (ast/syntax-tree
              [(ast/whitespace " " {:delimiters default-delimiters
                                    :row 1
                                    :column 1
                                    :contexts []})
               (ast/variable-tag ["x"] {:delimiters default-delimiters
                                        :row 1
                                        :column 2
                                        :standalone? true
                                        :contexts []})
               (ast/whitespace " " {:delimiters default-delimiters
                                    :row 1
                                    :column 7
                                    :contexts []})])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader "{{x}}\n{{y}}" 3)]
    (t/is (= (ast/syntax-tree
              [(ast/variable-tag ["x"] {:delimiters default-delimiters
                                        :row 1
                                        :column 1
                                        :standalone? true
                                        :contexts []})
               (ast/newline "\n" {:delimiters default-delimiters
                                  :row 1
                                  :column 6
                                  :contexts []})
               (ast/variable-tag ["y"] {:delimiters default-delimiters
                                        :row 2
                                        :column 1
                                        :standalone? true
                                        :contexts []})])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader "x{{x}} " 3)]
    (t/is (= (ast/syntax-tree
              [(ast/text "x" {:delimiters default-delimiters
                              :row 1
                              :column 1
                              :contexts []})
               (ast/variable-tag ["x"] {:delimiters default-delimiters
                                        :row 1
                                        :column 2
                                        :standalone? false
                                        :contexts []})
               (ast/whitespace " " {:delimiters default-delimiters
                                    :row 1
                                    :column 7
                                    :contexts []})])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader "{{foo}}{{bar}}" 3)]
    (t/is (= (ast/syntax-tree
              [(ast/variable-tag ["foo"] {:delimiters default-delimiters
                                          :row 1
                                          :column 1
                                          :standalone? false
                                          :contexts []})
               (ast/variable-tag ["bar"] {:delimiters default-delimiters
                                          :row 1
                                          :column 8
                                          :standalone? false
                                          :contexts []})])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader "{{#foo}}{{/foo}}" 3)]
    (t/is (= (ast/syntax-tree
              [(ast/section
                (ast/section-open-tag ["foo"] {:delimiters default-delimiters
                                               :row 1
                                               :column 1
                                               :standalone? false
                                               :contexts []})
                (ast/section-close-tag ["foo"] {:delimiters default-delimiters
                                                :row 1
                                                :column 9
                                                :standalone? false
                                                :contexts []}))])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader "{{#x}}{{#y}}{{/y}}{{/x}}" 3)]
    (t/is (= (ast/syntax-tree
              [(ast/section
                (ast/section-open-tag ["x"] {:delimiters default-delimiters
                                             :row 1
                                             :column 1
                                             :standalone? false
                                             :contexts []})
                (ast/section-close-tag ["x"] {:delimiters default-delimiters
                                              :row 1
                                              :column 19
                                              :standalone? false
                                              :contexts []})
                [(ast/section
                  (ast/section-open-tag ["y"] {:delimiters default-delimiters
                                               :row 1
                                               :column 7
                                               :standalone? false
                                               :contexts [["x"]]})
                  (ast/section-close-tag ["y"] {:delimiters default-delimiters
                                                :row 1
                                                :column 13
                                                :standalone? false
                                                :contexts [["x"]]})
                  [])])])
             (-> (sut/parse reader initial-state)
                 :ast))))

  (with-open [reader (test-reader "{{x}}hello" 3)]
    (t/is (= (ast/syntax-tree
              [(ast/variable-tag ["x"] {:delimiters default-delimiters
                                        :row 1
                                        :column 1
                                        :standalone? false
                                        :contexts []})
               (ast/text "hello" {:delimiters default-delimiters
                                  :row 1
                                  :column 6
                                  :contexts []})])
             (-> (sut/parse reader initial-state)
                 :ast)))))
