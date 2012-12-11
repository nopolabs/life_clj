(ns life.core)

; in repl: (use :reload 'life.core)

; The universe of the Game of Life is an infinite two-dimensional
; orthogonal grid of square cells, each of which is in one of
; two possible states, live or dead. Every cell interacts with 
; its eight neighbors, which are the cells that are directly 
; horizontally, vertically, or diagonally adjacent. 
;
; At each step in time, the following transitions occur:
;
;  Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;  Any live cell with more than three live neighbours dies, as if by overcrowding.
;  Any live cell with two or three live neighbours lives on to the next generation.
;  Any dead cell with exactly three live neighbours becomes a live cell.

; a cell is an location vector, e.g. [x y]
;
; a generation is a set of live cells

(defn input-str
  [in]
  (into #{}
    (for [[i row]  (map-indexed vector in)
          [j char] (map-indexed vector row)
          :when (= char \*)]
      [i j])))

(def gen-0 (input-str ["  *     *"
                       "* *   * *"
                       " **    **"]))

(defn input-matrix
  [matrix]
  (into #{}
    (let [rows (count matrix)
          cols (count (get matrix 0))]
      (for [i (range rows) j (range cols)
            :when (= \* (get-in matrix [i j]))]
        [i j]))))

(def gen-00 (input-matrix [[ \* \- \- ]
                           [ \* \- \* ]
                           [ \* \* \- ]]))

(defn min-max-location
  [[min-loc max-loc] loc]
  (let [[min-x min-y] min-loc
        [max-x max-y] max-loc
        [x y] loc]
  [[(min min-x x) (min min-y y)] [(max max-x x) (max max-y y)]]))

(defn bounds
  ([gen] (let [loc (first gen)] (bounds [loc loc] (rest gen))))
  ([min-max-loc gen]
    (if (empty? gen)
      min-max-loc
      (let [loc (first gen)]
        (bounds (min-max-location min-max-loc loc) (rest gen))))))

(defn output
  [gen]
  (let [[min-loc max-loc] (bounds gen)
        [min-x min-y] min-loc
        [max-x max-y] max-loc]
    (into []
      (for [r (range min-x (inc max-x))]
        (into []
          (for [c (range min-y (inc max-y))]
            (if (contains? gen [r c]) \* \-)))))))

(defn near [l] (range (dec l) (+ 2 l)))

(defn neighbors
  "list the neighbors of a given cell"
  [[x y]]
  (for [nx (near x)
        ny (near y)
        :when (not= [x y] [nx ny])]
    [nx ny]))

(defn gen-order [[x1 y1] [x2 y2]] (or (< x1 x2) (and (= x1 x2) (< y1 y2))))

(defn sort-gen [gen] (sort gen-order gen))

(defn all-neighbors [gen] (for [cell (sort-gen gen) n (neighbors cell)] n))

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

