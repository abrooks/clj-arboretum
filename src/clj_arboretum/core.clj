(ns clj-arboretum.core)

(defn- climb
  [so-far branch roots]
  (let [m-so-far (:subtrees (meta so-far))
        vconj (fnil conj [])]
    (with-meta (vconj so-far branch)
      {:subtrees (vconj m-so-far roots)} )))

(defn leaves
  "Climb a tree of nested associative data structures, returning their
  leaf nodes in as tuples of vector-of-path-elements and value. e.g.:
  (leaves {:a {:b {:c 1 :d 2}}})
  '([[:a :b :c] 1]
    [[:a :b :d] 2]
    ...) "
  [roots & [so-far]]
  (if (associative? roots)
    (for [[trunk branches] (cond (map? roots) roots
                                 :else (map-indexed vector roots))
          leaf (leaves branches (climb so-far trunk roots))]
      leaf)
    [[so-far roots]]))

(defn mulch
  "Analyze and throw helpful exception."
  [e roots leaves]
  (throw e))

(defn- grow
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
  ([leaves] (tree nil leaves))
  ([roots leaves]
     (try-debug mulch
       (loop [roots roots leaves leaves]
         (if (seq leaves)
           (let [roots (grow roots (first leaves))]
             (recur roots (rest leaves)))
           roots)))))


