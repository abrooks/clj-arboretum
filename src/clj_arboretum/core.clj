(ns clj-arboretum.core)

(def ^:private DNE (Object.))

(def ^:private ^:dynamic *vect-locs* nil)

(def vconj (fnil conj []))

(defn- climb
  [so-far branch roots]
  (let [m-so-far (:subtrees (meta so-far))]
    (with-meta (vconj so-far branch)
      {:subtrees (vconj m-so-far roots)} )))

(defn leaves
  "Climb a tree of nested associative data structures, returning their
  leaf nodes in as tuples of vector-of-path-elements and value. e.g.:
  (leaves {:a {:b {:c 1 :d [2 3]}}}) =>
  '([[:a :b :c] 1]
    [[:a :b :d 0] 2]
    [[:a :b :d 1] 3])"
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

(defn- handle-vect
  [roots subtree so-far]
  (when (= [] roots)
    (swap! *vect-locs* conj so-far))
  )

(defn- grow
  [tree [path leaf] & [so-far]]
  (if path
    (let [[trunk & nbranches] path
          [meta-trunk & meta-nbranches] (:subtrees (meta path))
          nbranches (when nbranches
                      (with-meta nbranches {:subtrees meta-nbranches}))
          tree (or tree (empty meta-trunk))
          more-tree (get tree trunk)
          so-far (vconj so-far trunk)
          subtree (grow more-tree [nbranches leaf] so-far)]
      (if (vector? tree)
        (handle-vect tree subtree so-far)
        (assoc tree trunk subtree)))
    leaf))

(defn- dead-wood
  [[roots vects]]
  (if (seq vects)
    (let []
        (recur after cleanup))
    root))

(defn tree
  ([leaves] (tree nil leaves))
  ([roots leaves]
     (try
       (->
        (loop [roots roots leaves leaves vects #{}]
          (if (seq leaves)
            (let [roots (grow roots (first leaves))]
              (recur roots (rest leaves)))
            [roots vects]))
        (dead-wood))
       (catch Exception e
         (mulch e roots leaves)))))


