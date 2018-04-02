; This demo uses the disp function from disp.lsp, and defines a macro sdisp.

; disp prints to the screen a single grid, a row of grids, or a table of grids. By default it prints one character per cell.

; sdisp is a macro that passes disp a function that allows it to display squashed grids, which contain two cells per character.
; As a result, squashed grids must have a height that is even to render properly.

(load :life.lisp)

(defun reload () (load :demo.lisp))


;; Set up initial grid

; 1. Generate a random grid

(defun random-row (n &optional (m 1) out)
    (if (= n 0)
        out
        (random-row (1- n) m (append out (list (random m))))
    )
)

(defun random-grid (width height sparse)
    (mapcar
        (lambda (x) (random-row width sparse))
        (make-list height :initial-element nil)))
    
; 2. Predefine a grid
    
; (defvar grid '(
    ; (0 0 0 0 0 0 0 0 0 0)
    ; (0 0 0 0 0 0 0 0 0 0)
    ; (0 0 0 0 0 0 0 0 0 0)
    ; (0 0 0 0 0 1 1 0 0 0)
    ; (0 0 0 0 0 1 0 1 0 0)
    ; (0 0 0 0 0 0 1 0 0 0)
    ; (0 0 0 0 0 0 0 0 0 0)
    ; (0 0 0 0 0 0 0 0 0 0)
    ; (0 0 0 0 0 0 0 0 0 0)
    ; (0 0 0 0 0 0 0 0 0 0)
; ))

; 3. Reshape a range and select numbered cells

; 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16

;  1  2  3  4 
;  5  6  7  8 
;  9 10 11 12 
; 13 14 15 16

;  1  1  0  0
;  1  0  1  0
;  0  1  0  0 
;  0  0  0  0

(setq grid
    (pad
        (reshape        
            (select '(2 5 6 7 10) (range 1 16))
            4
        )
        8 8 8 8
    )
)


;; Set up sample rows and tables

; Make a grid row
(setq g2
    (mapcar
        (lambda (n) (rotate-grid grid n 0))
        '(-1 0 1))
)

; Make a grid table
(setq g3
    (mapcar (lambda (n)
        (mapcar (lambda (g)
            (rotate-grid g 0 n))
            g2))
        '(-1 0 1))
)

; Squash the grid
(setq sgrid (squash grid))

; Squash the grid row
(setq sg2 (mapcar #'squash g2))

; Squash the grid table
(setq sg3 (mapcar (lambda (row)
    (mapcar #'squash row)) g3))


;; Display
    
; Show the squashed grid row
;(sdisp sg2)

; Show the grid table
;(disp g3)
