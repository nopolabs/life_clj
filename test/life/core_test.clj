(ns life.core-test
  (:use clojure.test
        life.core))

; in repl: (use :reload 'life.core-test)

(deftest neighbors-1-1-test
  (testing "neighbors [1 1]"
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
           [0 2] [0 3] [0 4]
           [1 2]       [1 4]
           [2 2] [2 3] [2 4]
           [1 2] [1 3] [1 4]
           [2 2]       [2 4]
           [3 2] [3 3] [3 4]
           [2 2] [2 3] [2 4]
           [3 2]       [3 4]
           [4 2] [4 3] [4 4]
          ]))))

