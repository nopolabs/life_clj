(ns life.core)

;; in repl: (use :reload 'life.core)

;; The universe of the Game of Life is an infinite two-dimensional
;; orthogonal grid of square cells, each of which is in one of
;; two possible states, live or dead. Every cell interacts with
;; its eight neighbors, which are the cells that are directly
;; horizontally, vertically, or diagonally adjacent.
;;
;; At each step in time, the following transitions occur:
;;
;;  Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;;  Any live cell with more than three live neighbours dies, as if by overcrowding.
;;  Any live cell with two or three live neighbours lives on to the next generation.
;;  Any dead cell with exactly three live neighbours becomes a live cell.

;; a cell is an location vector, e.g. [row col]
;;
;; a generation is a set of live cells

(def live-cell \*)

(def empty-cell \-)

(defn live-cell? [c] (= live-cell c))

(defn empty-cell? [c] (complement (live-cell? c)))

(defn input-str
  [in]
  (into #{}
    (for [[r row]  (map-indexed vector in)
          [c char] (map-indexed vector row)
          :when (live-cell? char)]
      [r c])))

(def gen-0 (input-str ["    "
                       "   *"
                       " * *"
                       "  **"]))

(defn input-matrix
  [matrix]
  (into #{}
    (let [rows (count matrix)
          cols (count (get matrix 0))]
      (for [r (range rows)
	    c (range cols)
            :when (live-cell? (get-in matrix [r c]))]
        [r c]))))

(def gen-00
     (input-matrix [[ \* \- \- ]
                    [ \* \- \* ]
                    [ \* \* \- ]]))

(defn vector-map
  [f v1 v2]
  (map #(let [[x y] %] (f x y))
       (map vector
	    v1 v2)))

(defn vector-min
  [v1 v2]
  (vector-map min v1 v2))

(defn vector-max
  [v1 v2]
  (vector-map max v1 v2))

(defn bounds
  ([locs]
     (let [loc (first locs)]
       (bounds [loc loc] (rest locs))))
  ([[top-left bottom-right] locs]
    (if (empty? locs)
      [top-left bottom-right]
      (let [loc (first locs)
            tl (vector-min loc top-left)
            br (vector-max loc bottom-right)]
        (bounds [tl br] (rest locs))))))

(defn output
  [gen]
  (let [[top-left bottom-right] (bounds gen)
        [min-row min-col] top-left
        [max-row max-col] bottom-right]
    (into []
      (for [r (range min-row (inc max-row))]
        (into []
          (for [c (range min-col (inc max-col))]
            (if (contains? gen [r c])
	      live-cell
	      empty-cell)))))))

(defn near [l] (range (dec l) (+ 2 l)))

(defn neighbors
  "list the neighbors of a given cell"
  [loc]
  (let [[r c] loc]
    (for [nr (near r)
          nc (near c)
          :when (not= [r c] [nr nc])]
      [nr nc])))

(defn all-neighbors
  [gen]
  (apply concat 
    (for [loc gen]
      (neighbors loc))))

(defn gen-order [[r1 c1] [r2 c2]] (or (< r1 r2) (and (= r1 r2) (<= c1 c2))))

(defn sort-gen [gen] (sort gen-order gen))

(defn count-map [m] (into {} (for [[k v] m] [k (count v)])))

(defn neighborhood-map [gen] (count-map (group-by identity (all-neighbors gen))))

(defn survivor? [cell n gen] (and (contains? gen cell) (> n 1) (< n 4)))

(defn birth? [cell n gen] (and (not (contains? gen cell)) (= n 3)))

(defn live? [cell n gen] (or (survivor? cell n gen) (birth? cell n gen)))

(defn next-gen
  "next generation"
  [gen]
  (set 
    (let [m (neighborhood-map gen)]
      (for [cell (keys m)
            :when (live? cell (m cell) gen)]
        cell))))

(defn ngen
  ([gen] (let [hood (neighborhood-map gen)] (ngen gen hood (keys hood) ())))
  ([gen hood cells ng]
    (if (empty? cells)
      (set ng)
      (let [f (first cells)
            r (rest cells)
            c (hood f)
            l (if (live? f c gen) (cons f ng) ng)]
          (ngen gen hood r l)))))

(defn ngenr
  [gen]
  (let [hood (neighborhood-map gen)]
    (loop [cells (keys hood)
           live ()]
      (if (empty? cells)
        (set live)
        (let [f (first cells)
              c (hood f)
              l (if (live? f c gen) (cons f live) live)]
          (recur (rest cells) l))))))
          
(defn nth-gen
  "nth generation"
  ([number generation] (nth-gen next-gen number generation))
  ([gen-fn number generation]
    (loop [n number
           gen generation]
      (if (= 0 n)
        (sort-gen gen)
        (recur (dec n) (gen-fn gen))))))

