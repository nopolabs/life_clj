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

; a cell is an [x y] vector
;
; a generation is a set of live cells
(def gen-0 #{            [1 3]
             [2 1]       [2 3]
                   [3 2] [3 3] })

(defn near [l] (range (dec l) (+ 2 l)))

(defn neighbors
  "list the neighbors of a given cell"
  [[x y]]
  (for [nx (near x)
        ny (near y)
        :when (not= [x y] [nx ny])]
    [nx ny]))

(defn gen-order [[x1 y1] [x2 y2]] (or (< x1 x2) (< y1 y2)))

(defn sort-gen [gen] (sort gen-order gen))

(defn all-neighbors [gen] (for [cell (sort-gen gen) n (neighbors cell)] n))

(defn neighborhood-map [gen] (group-by identity (all-neighbors gen)))

(defn survivor? [cell n gen] (and (contains? gen cell) (> n 1) (< n 4)))

(defn birth? [cell n gen] (and (not (contains? gen cell)) (= n 3)))

(defn live? [cell n gen] (or (survivor? cell n gen) (birth? cell n gen)))

(defn next-gen
  "next generation"
  [gen]
  (set 
    (let [m (neighborhood-map gen)]
      (for [cell (keys m)
            :when (live? cell (count (m cell)) gen)]
            cell))))

(defn nth-gen
  "nth generation"
  [number generation]
  (loop [n number
         gen generation]
    (if (= 0 n)
      (sort-gen gen)
      (recur (dec n) (next-gen gen)))))
