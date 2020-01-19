(ns org.panchromatic.mokuhan2.util.misc)

(defn deep-merge [m1 m2]
  (cond
    (and (map? m1) (map? m2))
    (merge-with deep-merge m1 m2)

    (and (some? m1) (nil? m2))
    m1

    :else
    m2))

(deftype FixedLengthVector [n v]
  clojure.lang.IPersistentCollection
  (count [_]
    n)
  (cons [_ x]
    (FixedLengthVector. n (subvec (conj v x) 1)))
  (empty [_]
    false)
  (equiv [_ x]
    (and (instance? FixedLengthVector x)
         (= n (.n x))
         (= v (.v x)))))

(defn fixed-legnth-vector
  ([n]
   (fixed-legnth-vector n nil))
  ([n init-val]
   {:pre [(pos-int? n)]}
   (FixedLengthVector. n (vec (repeat n init-val)))))
