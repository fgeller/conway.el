;; let's play with TDD in elisp.

(defun cell (x y) (cons x  y))
(defun cell-x (c) (car c))
(defun cell-y (c) (cdr c))

(defun cell-neighbors (cell)
  (let (neighbors)
    (dolist (x-offset '(-1 0 1))
      (dolist (y-offset '(-1 0 1))
        (let ((new-cell (cell (- (cell-x cell) x-offset)
                              (- (cell-y cell) y-offset))))
          (when (not (equal new-cell cell))
            (add-to-list 'neighbors new-cell)))))
    neighbors))

(defun board (cells) cells)

(defun board-living-cell-p (board x y)
  (member (cell x y) board))

(defun board-cell-will-live-p (board cell)
  (let ((living-neighbor-count 0))
    (dolist (neighbor (cell-neighbors cell))
      (when (board-living-cell-p board (cell-x neighbor) (cell-y neighbor))
        (setq living-neighbor-count (1+ living-neighbor-count))))
    (or
     (and (equal 2 living-neighbor-count)
          (board-living-cell-p board (cell-x cell) (cell-y cell)))
     (equal 3 living-neighbor-count))))

(defun board-limits (board)
  (let (max-x min-x max-y min-y)
    (dolist (cell board)
      (setq max-x (if (or (not max-x) (< max-x (cell-x cell))) (cell-x cell) max-x))
      (setq min-x (if (or (not min-x) (> min-x (cell-x cell))) (cell-x cell) min-x))
      (setq max-y (if (or (not max-y) (< max-y (cell-y cell))) (cell-y cell) max-y))
      (setq min-y (if (or (not min-y) (> min-y (cell-y cell))) (cell-y cell) min-y)))
    `(:max-x ,max-x :min-x ,min-x :max-y ,max-y :min-y ,min-y)))

(defun board-tick (board)
  (let ((limits (board-limits board))
        next-cells)
    (dolist (iter-y (number-sequence (1- (plist-get limits :min-y)) (1+ (plist-get limits :max-y))))
      (dolist (iter-x (number-sequence (1- (plist-get limits :min-x)) (1+ (plist-get limits :max-x))))
        (let ((next-cell (cell iter-x iter-y)))
          (when (board-cell-will-live-p board next-cell)
            (setq next-cells (cons next-cell next-cells))))))
    (board (reverse next-cells))))

(defun board-to-string (board)
  (let ((limits (board-limits board)) (str ""))
    (dolist (iter-y (number-sequence (1- (plist-get limits :min-y)) (1+ (plist-get limits :max-y))))
      (dolist (iter-x (number-sequence (1- (plist-get limits :min-x)) (1+ (plist-get limits :max-x))))
        (let ((next-cell (cell iter-x iter-y)))
          (if (board-living-cell-p board (cell-x next-cell) (cell-y next-cell))
              (setq str (concat str "#"))
            (setq str (concat str " ")))))
      (setq str (concat str "\n")))
    str))

(defun conways-game-of-life-tick (buf next-board)
  (with-current-buffer buf
    (erase-buffer)
    (insert (board-to-string next-board))
    (redisplay t)
    (run-with-timer 0.1 nil 'conways-game-of-life-tick buf (board-tick next-board))))

(defun conways-game-of-life ()
  (interactive)
  (let* ((buf (generate-new-buffer "*conways-game-of-life"))
         (next-board (board (list

                             (cell 0 0)
                                                (cell 2 1)
                 (cell -1 2) (cell 0 2)                    (cell 3 2) (cell 4 2) (cell 5 2)
                ;;             (cell 0 0) (cell 1 0) (cell 2 0)
                ;; (cell -1 1) (cell 0 1) (cell 1 1)


                ;;             (cell 0 4) (cell 1 4) (cell 2 4)
))))
    (switch-to-buffer buf)
    (conways-game-of-life-tick buf next-board)))


(ert-run-tests-interactively "^conway-test" " *ert conway-tests*")


(ert-deftest conway-test:board-to-string ()
  (let* ((test-board (board (list
                             (cell 0 0) (cell 1 0)
                             (cell 0 1))))
         (next-board (board (list
                             (cell 0 0) (cell 1 0)
                             (cell 0 1) (cell 1 1)))))
    (cl-assert (equal "    
 ## 
 #  
    
" (board-to-string test-board)))
    (cl-assert (equal "    
 ## 
 ## 
    
" (board-to-string next-board)))))


(ert-deftest conway-test:board-tick ()
  (let* ((test-board (board (list
                             (cell 0 0) (cell 1 0)
                             (cell 0 1))))
         (next-board (board (list
                             (cell 0 0) (cell 1 0)
                             (cell 0 1) (cell 1 1)))))
    (cl-assert (equal next-board (board-tick test-board)))
    (cl-assert (equal next-board (board-tick next-board)))))


(ert-deftest conway-test:board-cell-will-live ()
  (let* ((test-board (board (list
                             (cell 0 0) (cell 1 0) (cell 2 0)
                             (cell 0 1)            (cell 2 1) (cell 3 1)
                                                   (cell 2 2)))))
    (cl-assert (not (board-cell-will-live-p test-board (cell 2 -1))))
    (cl-assert (board-cell-will-live-p test-board (cell 0 0)))
    (cl-assert (not (board-cell-will-live-p test-board (cell 1 0))))
    (cl-assert (board-cell-will-live-p test-board (cell 3 0)))
    (cl-assert (board-cell-will-live-p test-board (cell 0 1)))
    (cl-assert (not (board-cell-will-live-p test-board (cell 1 1))))
    (cl-assert (not (board-cell-will-live-p test-board (cell 2 1))))
    (cl-assert (board-cell-will-live-p test-board (cell 3 2)))))


(ert-deftest conway-test:cell-neighbors ()
  (let* ((test-cell (cell 1 1))
         (neighbors (cell-neighbors test-cell)))
    (cl-assert (equal 8 (length neighbors)))))


(ert-deftest conway-test:board-knows-living-cell ()
  (let* ((test-cell (cell 1 1))
         (test-board (board (list test-cell))))
    (cl-assert (not (board-living-cell-p test-board 0 0)))
    (cl-assert (board-living-cell-p test-board (cell-x test-cell) (cell-x test-cell)))))


(ert-deftest conway-test:cell-coordinates ()
  (let ((test-cell (cell 1 1)))
    (cl-assert (equal 1 (cell-x test-cell)))
    (cl-assert (equal 1 (cell-y test-cell)))))
