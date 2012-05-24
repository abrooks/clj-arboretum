(ns clj-arboretum.core)

(defn plant
  [c]
  (if (map? c)
    c
    (map-indexed vector c)))

(defn climb
  [so-far branch roots]
  (let [mso-far (:subtrees (meta so-far))
        vconj (fnil conj [])]
    (with-meta (vconj so-far branch)
      {:subtrees (vconj mso-far roots)} )))

(defn leaf
  "Climb a tree of nested associative data structures, returning their
  leaf nodes in as map values, indexed by a vector of the keys to
  reach those leaf values."
  [roots & [so-far]]
  (if (associative? roots)
    (for [[trunk branches] (plant roots)
          leaves (leaf branches (climb so-far trunk roots))]
      leaves)
    [[so-far roots]]))

(defn mulch
  "Analyze and throw helpful exception."
  [e leaves]
  (throw e))

(defn grow
  [roots [branches leaf]]
  (if branches
    (let [[trunk & nbranches] branches
          [m-trunk & m-nbranches] (:subtrees (meta branches))
          nbranches (when nbranches
                  (with-meta nbranches {:subtrees m-nbranches}))
          roots (or roots (empty m-trunk))
          nroots (get roots trunk)]
      (assoc roots trunk (grow nroots [nbranches leaf])))
    leaf))

(defn tree
  [leaves]
  (try
    (loop [roots nil leaves leaves]
      (if (seq leaves)
        (let [roots (grow roots (first leaves))]
          (recur roots (rest leaves)))
        roots))
    (catch Exception e
      (mulch e leaves))))
