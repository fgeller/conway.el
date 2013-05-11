;; let's play with TDD in elisp.

(defun cell (x y) (cons x  y))
(defun cell-x (c) (car c))
(defun cell-y (c) (cdr c))

(defun board (cells) cells)
(defun board-living-cell-p (board x y)
  (when board
    (let ((next-cell (car board)))
      (or (and (equal x (cell-x next-cell)) (equal y (cell-y next-cell)))
          (board-living-cell-p (cdr board) x y)))))


(ert-run-tests-interactively "^conway-test" " *ert conway-tests*")


(ert-deftest conway-test:board-knows-living-cell ()
  (let* ((test-cell (cell 1 1))
         (test-board (board (list test-cell))))
    (cl-assert (not (board-living-cell-p test-board 0 0)))
    (cl-assert (board-living-cell-p test-board (cell-x test-cell) (cell-x test-cell)))))


(ert-deftest conway-test:cell-coordinates ()
  (let ((test-cell (cell 1 1)))
    (cl-assert (equal 1 (cell-x test-cell)))
    (cl-assert (equal 1 (cell-y test-cell)))))
