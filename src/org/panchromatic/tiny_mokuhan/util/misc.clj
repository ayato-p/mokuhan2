(ns org.panchromatic.tiny-mokuhan.util.misc)

(defn deep-merge [m1 m2]
  (cond
    (and (map? m1) (map? m2))
    (merge-with deep-merge m1 m2)

    (and (some? m1) (nil? m2))
    m1

    (and (some? m2) (nil? m1))
    m2))
