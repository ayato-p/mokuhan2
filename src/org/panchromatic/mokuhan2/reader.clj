(ns org.panchromatic.mokuhan2.reader)

(defn pushback-reader
  ([^java.io.Reader in]
   (pushback-reader in 1))
  ([^java.io.Reader in n]
   (java.io.PushbackReader. in n)))

(defn read-char [^java.io.Reader rdr]
  (let [i (.read rdr)]
    (when-not (neg? i)
      (char i))))

(defn read-chars [^java.io.Reader rdr n]
  (loop [n n
         res []]
    (if-not (pos? n)
      res
      (recur (dec n) (conj res (read-char rdr))))))

(defn skip [^java.io.Reader rdr n]
  (.skip rdr n))

(defn unread-char
  [^java.io.PushbackReader rdr char]
  (when char
    (.unread rdr (int char))))

(defn unread-chars
  [^java.io.PushbackReader rdr chrs]
  (doseq [c (reverse chrs)]
    (unread-char rdr c)))
