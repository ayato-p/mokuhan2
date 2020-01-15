(ns org.panchromatic.mokuhan2.helper)

(defmacro import-private-var
  ([qsym]
   `(if (qualified-symbol? ~qsym)
      (import-private-var (symbol (namespace ~qsym)) (symbol (name ~qsym)))
      (throw (IllegalArgumentException. "qsym should be qualified symbol"))))
  ([ns-sym name-sym]
   `(intern (ns-name *ns*)
            ~name-sym
            (ns-resolve ~ns-sym ~name-sym))))

(defmacro import-private-vars [qsyms]
  `(do
     ~@(for [qsym qsyms]
         `(import-private-var ~qsym))))
