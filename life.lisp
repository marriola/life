(load :disp.lisp)

(defun range (start count &optional out)
    (if (= count 0)
        out
        (range (1+ start) (1- count) (append out (list start)))
    )
)

(defun reshape (xs width &optional (start 0) out)
    (cond
        ((null xs)
            out)
        ((> (- (length xs) start) width)
            (let ((next (+ start width)))
                (reshape
                    xs width
                    next
                    (append
                        out
                        (list (subseq xs start next))
                    )
                )
            )
        )
        (t (append out (list (subseq xs start))))
    )
)

(defun pad (g up right down left)
    (let*
        (
            (blank-line
                (make-list
                    (+ left right (length (car g)))
                    :initial-element 0))
            (fill-up
                (make-list up :initial-element blank-line))
            (fill-down
                (make-list down :initial-element blank-line))
            (fill-left (make-list left :initial-element 0))
            (fill-right (make-list right :initial-element 0))
        )
        (append
            fill-up
            (mapcar
                (lambda (r) (append fill-left r fill-right))
                g)
            fill-down
        )
    )
)

(defun rotate (seq offset)
    (cond
        ((= offset 1)
            (append (last seq) (reverse (rest (reverse seq))))
        )
        ((= offset -1)
            (append (rest seq) (list (first seq)))
        )
        (t seq)
    )
)

(defun rotate-grid (grid offset-x offset-y)
    (rotate
        (mapcar (lambda (r) (rotate r offset-x)) grid)
        offset-y
    )
)

(defun select (select-items xs)
    (mapcar (lambda (x) (if (find x select-items) 1 0)) xs)
)

(defun neighbor-count (grid)
    "Computes the neighbor count for each cell of a grid"
    (let*
        ; Produce an array of each row rotation
        ((row-rotation
            (mapcar
                (lambda (n) (rotate-grid grid n 0))
            '(-1 0 1)))
        ; Produce an array of each column rotation over each row rotation
        (column-rotation
            (mapcar (lambda (n)
                (mapcar (lambda (g)
                    (rotate-grid g 0 n))
                    row-rotation))
                '(-1 0 1))))
        ; Sum the rows, then the resulting columns for the neighbor count
        (row-sum (mapcar #'row-sum column-rotation))
    )
)

(defun row-sum (grid-row)
    "Sums an array of matrices"
    (let*
        ; Tuple each row of each grid together
        ((tuple-grid (apply #'mapcar (append `(,#'list) grid-row))))
        ; Apply + across each tuple of rows, yielding the sum
        (mapcar
            (lambda (r)
                (apply #'mapcar (append `(,#'+) r)))
            tuple-grid)
    )
)

(setq clear-screen-escape (format nil "~c[2J" #\Escape))
(setq home-escape (format nil "~A[H" #\Escape))

(defun life-n (grid generations &optional (delay 0.25))
    "Plays Conway's Game of Life for N generations"
    (format t clear-screen-escape)
    (loop for i from 1 to generations do
        (format t home-escape)
        (setq grid (next-gen grid))
        (disp grid)
        (sleep delay)
    )
)

(defun life (grid &optional (delay 0.25))
    "Plays Conway's Game of Life indefinitely"
    (format t clear-screen-escape)
    (loop
        (format t home-escape)
        (setq grid (next-gen grid))
        (sdisp (squash grid))
        (sleep delay)
    )
)

(defun next-gen (grid)
    "Computes the next generation of the grid"
    (let*
        (
            (grid-height (length grid))
            (neighbor-grid (neighbor-count grid))
            (threes
                (through
                    (highlight 3)
                    neighbor-grid))
            (fours
                (through
                    (highlight 4)
                    neighbor-grid))
            (fours-and-original
                ; Pair each row of fours with each row of the original grid, and logand each pair of rows together.
                (mapcar
                    (lambda (pair) (mapcar #'logand (first pair) (second pair)))
                    (mapcar #'list grid fours)))
        )
        ; Pair each row of of threes and fours-and-original together and logior each pair of rows together
        (mapcar
            (lambda (pair) (mapcar #'logior (first pair) (second pair)))
            (mapcar #'list threes fours-and-original))
    )
)

(defun through (fun m)
    "Applies a function to each scalar element of arbitrarily nested lists"
    (if (listp (car m))
        (mapcar (lambda (n) (through fun n)) m)
        (mapcar fun m)))

(defmacro highlight (n)
    "Returns a lambda function that returns 1 if given N, and 0 if given any other value"
    `(lambda (m) (if (= m ,n) 1 0)))

(defun count-char (c)
    "Displays the neighbor count for a cell"
    (if (zerop c)
        "∙"
        (write-to-string c))
)
