(ns game-of-life.core)

(def board-size 10)
(def board-area (* board-size board-size))

(def board (take board-area (repeatedly #(rand-int 2))))

(def cells-coords
  (for [x (range board-size), y (range board-size)]
    [x y]))

(defn step
  "Make the whole board advance a step"
  [board]
  (map #(step-cell board %) cells-coords))

(defn step-cell
  "Returns the next step of a cell, based on the actual rules of the game of life"
  [board [x y]]
  (let [live-neighbors (count-live-neighbors board x y)]
    (if (or (= 3 live-neighbors)
            (and (= 2 live-neighbors)
                 (alive? board x y)))
      1 0)))

(defn count-live-neighbors
  "Given a board and a cell's x & y coordinates, return the number of live neighbors"
  [board x y]
  (->> (neighbors x y)
       (map (fn [[x y]] (cell-at board x y)))
       (reduce +)))

(defn neighbors
  "Returns the actual neighbors of a cell that are within the bounds of the board"
  [x y]
  (filter inside-bounds? (neighbors-coords x y)))

(defn inside-bounds?
  "Indicates if a given [x y] coordinate is inside the bounds of the board"
  [[x y]]
  (and (>= x 0) (>= y 0) (< x board-size) (< y board-size)))

(defn neighbors-coords
  "Returns the coordinates of the eight neighbors of a cell"
  [x y]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
      [(+ x dx) (+ y dy)]))

(defn cell-at
  "Inside a given board, returns the value of the cell at coords x and y"
  [board x y]
  (if (or (< x 0) (< y 0))
    (throw (Exception. "Cannot access cell at x or y < 0"))
    (nth board (+ x (* 10 y)))))

(defn alive?
  "Indicates whether a cell is alive or not in a given board"
  [board x y]
  (= 1 (cell-at board x y)))
