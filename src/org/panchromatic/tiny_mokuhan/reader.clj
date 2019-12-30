(ns org.panchromatic.tiny-mokuhan.reader)

(defn pushback-reader
  ([^java.io.Reader in]
   (pushback-reader in 1))
  ([^java.io.Reader in n]
   (java.io.PushbackReader. in n)))

(defn read-char [^java.io.Reader rdr]
  (let [i (.read rdr)]
    (when-not (neg? i)
      (char i))))

(defn unread-char
  [^java.io.PushbackReader rdr char]
  (.unread rdr (int char)))
