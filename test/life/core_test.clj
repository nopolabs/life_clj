(ns life.core-test
  (:use clojure.test
        life.core))

; in repl: (use :reload 'life.core-test)

(deftest v-map-test
  (testing "v-map"
    (is (= (v-map + [1 2 3] [4 5 6]) [5 7 9]))))

(deftest v-min-test
  (testing "v-min"
    (is (= (v-min [1 2 3] [3 2 1]) [1 2 1]))))

(deftest v-max-test
  (testing "v-max"
    (is (= (v-max [1 2 3] [3 2 1]) [3 2 3]))))

(deftest bounds-test
  (testing "bounds"
    (is (= (bounds [[1 2 3] [3 1 2] [3 2 0]]) [[1 1 0] [3 2 3]]))))

(deftest output-test
  (testing "output"
    (is (= (output gen-0) [[\- \- \*] [\* \- \*] [\- \* \*]]))
    (is (= (output gen-00) [[\* \- \-] [\* \- \*] [\* \* \-]]))))

(deftest near-test
  (testing "near"
    (is (= (near 1) [0 1 2])) 
    (is (= (near 2) [1 2 3])))) 

(deftest neighbors-test
  (testing "neighbors"
    (is (= (neighbors [1 1])
            [[0 0] [0 1] [0 2]
             [1 0]       [1 2]
             [2 0] [2 1] [2 2]]))))

(deftest all-neighbors-test
  (testing "all-neighbors"
    (is (=
          (all-neighbors gen-0)
          [
           [1 0] [1 1] [1 2]
           [2 0]       [2 2]
           [3 0] [3 1] [3 2]
           [2 1] [2 2] [2 3]
           [3 1]       [3 3]
           [4 1] [4 2] [4 3]
           [2 2] [2 3] [2 4]
           [3 2]       [3 4]
           [4 2] [4 3] [4 4]
           [1 2] [1 3] [1 4]
           [2 2]       [2 4]
           [3 2] [3 3] [3 4]
           [0 2] [0 3] [0 4]
           [1 2]       [1 4]
           [2 2] [2 3] [2 4]
          ]))))

(deftest gen-order-test
  (testing "gen-order"
    (is (gen-order [0 0] [0 0]))
    (is (gen-order [0 0] [0 1]))
    (is (gen-order [0 0] [1 0]))
    (is (gen-order [0 0] [1 1]))
    (is (not (gen-order [0 1] [0 0])))
    (is (gen-order [0 1] [0 1]))
    (is (gen-order [0 1] [1 0]))
    (is (gen-order [0 1] [1 1]))
    (is (not (gen-order [1 0] [0 0])))
    (is (not (gen-order [1 0] [0 1])))
    (is (gen-order [1 0] [1 0]))
    (is (gen-order [1 0] [1 1]))
    (is (not (gen-order [1 1] [0 0])))
    (is (not (gen-order [1 1] [0 1])))
    (is (not (gen-order [1 1] [1 0])))
    (is (gen-order [1 1] [1 1]))))

