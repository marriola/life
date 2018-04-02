(defun disp (g &optional (render #'grid-char))
    (case (- (depth g) 2)
        (0
            (print-grid g
                :sides '(t t t t)
                :join '(nil nil nil nil)
                :render render))
        (1
            (format t
                (build-grid-row g
                    :sides '(t t t t)
                    :render render)))
        (2
            (format t (build-grid g :render render)))
        (otherwise
            (error "Grid depth > 2 not supported"))
    )
)

(defmacro sdisp (grid) `(disp ,grid #'squashed-char))

(defun depth (x &optional (d 0))
    (if (listp x)
        (depth (first x) (1+ d))
        d
    )
)

(defun build-grid (grid-rows &key render)
    (let*
        (
            (length-minus-1 (1- (length grid-rows)))
            (top (append '(t) (make-list length-minus-1 :initial-element nil)))
            (join-vert (append (make-list length-minus-1 :initial-element '(nil t)) (list '(t nil))))
        )
        (format nil "~{~a~}"
            (mapcar
                (lambda (grid-row top join-vert)
                    (build-grid-row grid-row :sides (list top t t t) :join-vert join-vert :render render)
                )
                grid-rows
                top
                join-vert
            )
        )
    )
)

(defun build-grid-row (grids &key (sides '(nil nil nil nil)) (join-vert '(nil nil)) render)
    "Builds a display grid row"
    (let*
        (
            (top (nth 0 sides))
            (right (nth 1 sides))
            (bottom (nth 2 sides))
            (left (nth 3 sides))
            (height (+
                (if top 1 0)
                (if bottom 1 0)
                (length (first grids))))
            (out-str "")
            (length-minus-1 (1- (length grids)))
            (join-left
                (append '(nil) (make-list length-minus-1 :initial-element t)))
            (join-right
                (append (make-list length-minus-1 :initial-element t) '(nil)))
            (join-grids (mapcar #'list grids join-left join-right))
            (ascii-join-grids (map-grid-row join-grids top right bottom left join-vert :render render))
        )
        (loop for i from 1 to height do
            (loop for g in ascii-join-grids do
                (setq out-str (concatenate 'string out-str (line g i))))
            (setq out-str (concatenate 'string out-str '(#\Newline)))
        )
        out-str
    )
)

(defun map-grid-row (grids top right bottom left join-vert &key (out nil) render)
    "Renders a row of life grids to ASCII with the specified borders"
    (if (null grids)
        out
        (let*
            (
                (join-grid (first grids))
                (grid (nth 0 join-grid))
                (join-left (nth 1 join-grid))
                (join-right (nth 2 join-grid))
                (join-up (first join-vert))
                (join-down (second join-vert))
                (next-grid
                    (print-grid
                        grid
                        :dest nil
                        :sides (list top right bottom left)
                        :join (list join-up join-right join-down join-left)
                        :render render))
            )
            (map-grid-row
                (rest grids)
                ; hide left border for all grids following the first
                top right bottom nil
                join-vert
                :out (append out (list next-grid))
                :render render
            )
        )
    )
)

(defun line (str num &key (start 0))
    (let*
        (
            (next-newline (position #\Newline str :start start))
            (substring (subseq str start next-newline))
        )
        (cond
            ((= num 1) substring)
            ((null next-newline) "")
            (t (line str (1- num) :start (1+ next-newline)))
        )
    )
)

(defun print-grid (grid &key (dest t) (render #'grid-char) sides join)
    (let
        (
            (top (nth 0 sides))
            (right (nth 1 sides))
            (bottom (nth 2 sides))
            (left (nth 3 sides))
            (join-up (nth 0 join))
            (join-right (nth 1 join))
            (join-down (nth 2 join))
            (join-left (nth 3 join))
            (width (length (first grid)))
        )
        (format dest
            (concatenate 'string
                (if top
                    (grid-end
                        width left right
                        (take-if-not-null
                            (if join-up 'up)
                            (if join-right 'right)
                            'down
                            (if join-left 'left))))
                (reduce
                    (lambda (x y) (concatenate 'string x y))
                    (mapcar (lambda (r) (build-row render r left right)) grid)            
                    :initial-value ""
                )
                (if bottom
                    (grid-end
                        width left right
                        (take-if-not-null
                            'up
                            (if join-right 'right)
                            (if join-down 'down)
                            (if join-left 'left))))
            )
        )
    )
)

(defun grid-char (c)
    (if (zerop c) "∙" "█")
)

(defun build-row (render row left right)
    (let ((format-string
        (cond
            ((and left right) "│~a│~%")
            (left "│~a~%")
            (right "~a│~%")
            (t "~a~%")
        )))
        (format nil format-string
            (reduce
                (lambda (x y) (concatenate 'string x (funcall render y)))
                row
                :initial-value ""
            )
        )
    )
)

(defun grid-end (width left right &optional join)
    (let
        (
            (left-char
                (if (not left) #\Null (box-char (cdir join 'right))))
            (right-char
                (if (not right) #\Null (box-char (cdir join 'left))))
        )
        (format nil "~c~{~A~}~c~%"
            left-char
            (make-list width :initial-element #\box_drawings_light_horizontal)
            right-char)
    )
)

(defun cdir (d add)
    (let
        (
            (up (find 'up d))
            (right (find 'right d))
            (down (find 'down d))
            (left (find 'left d))
        )
        (take-if-not-null
            (if (or up (eq add 'up)) 'up)
            (if (or right (eq add 'right)) 'right)
            (if (or down (eq add 'down)) 'down)
            (if (or left (eq add 'left)) 'left)
        )
    )
)

(defun box-char (dir)
    (or (item dir vertices) #\Null)
)

(defvar vertices '(
    ((right down) . #\box_drawings_light_down_and_right)
    ((right down left) . #\box_drawings_light_down_and_horizontal)
    ((down left) . #\box_drawings_light_down_and_left)
    ((up right down) . #\box_drawings_light_vertical_and_right)
    ((up right down left) . #\box_drawings_light_vertical_and_horizontal)
    ((up down left) . #\box_drawings_light_vertical_and_left)
    ((up left) . #\box_drawings_light_up_and_left)
    ((up right left) . #\box_drawings_light_up_and_horizontal)
    ((up right) . #\box_drawings_light_up_and_right)
))

(defmacro take-if-not-null (&rest xs) `(remove-if #'null (list ,@xs)))

(defmacro item (key a-list) `(rest (assoc ,key ,a-list :test #'equal)))

; Compositing rows

(defun squash (g)
    (mapcar #'squash-row-pair (chunk g 2))
)

(defun chunk (xs size &optional out)
    (if (null xs)
        out
        (let*
            (
                (chunk (first (last out)))
                (chunk-size (length chunk))
            )
            (chunk
                (rest xs) size
                (if (= size chunk-size)                    
                    (append
                        out
                        (list (list (first xs))))
                    (append
                        (butlast out)
                        (list
                            (append
                                chunk
                                (list (first xs)))))
                )
            )
        )
    )
)

(defun squash-row-pair (pair)
    (mapcar #'squash-cells
        (mapcar #'list (first pair) (second pair))
    )
)

(defun squash-cells (cell)
    (let
        ((top (second cell))
        (bottom (first cell)))
        (logior (ash top 1) bottom)
    )
)

(defun squashed-char (c)
    "Displays a squashed cell"
    (case c
        (0 " ")
        (1 "▀")
        (2 "▄")
        (3 "█")
    )
)
