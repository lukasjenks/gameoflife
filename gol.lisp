;;; Function that determines the life or death of the current cell in the next gen
(defun future (cell cur-gen)
  (let ((cellAliveTest (is-alive cell cur-gen)) (neighAliveCount (neigh-alive cell curr-gen)))
	(cond	((and (= cellAliveTest 1)(= neighAliveCount 1)) nil) ;dies
			((and (= cellAliveTest 1)(= neighAliveCount 3)) nil) ;dies
			((and (= cellAliveTest 1)(= neighAliveCount 2)) t)   ;survives
			((and (= cellAliveTest 1)(= neighAliveCount 4)) t)   ;survives
			((and (= cellAliveTest 0)(= neighAliveCount 1)) nil) ;stays dead
			((and (= cellAliveTest 0)(= neighAliveCount 4)) nil) ;stays dead
			((and (= cellAliveTest 0)(= neighAliveCount 2)) t)   ;born
			((and (= cellAliveTest 0)(= neighAliveCount 3)) t)   ;born
			(t nil)))) ;otherwise stay dead

;;; Function to test if cell is alive
(defun is-alive (cell curr-gen)
  (cond ((null cur-gen) 0)
		((= cell (car curr-gen)) 1)
		(t (is-alive cell (cdr curr-gen)))))

;;; Function to count live neighbours
(defun neigh-alive (cell curr-gen)
  (cond ((null curr-gen) 0)
		(t (+ (is-neigh cell (car curr-gen))(neigh-alive cell (cdr curr-gen))))))

;;; Function to limit neighbours to the two elements on either side of the current cell
(defun is-neigh (cell potential)
  (cond ((= potential (- cell 2)) 1)
		((= potential (- cell 1)) 1)
		((= potential (+ cell 1)) 1)
		((= potential (+ cell 2)) 1)
		(t 0)))

;;; Function to return positions of live cells
;;; TODO: Stop comparing cell to whole list to make more efficient
(defun life-array (cell curr-gen)
  ;; let cell-future = dead or alive (nil or t)
  (let ((cell-future (future cell curr-gen)))
	;; halting condition is two positions after the last element of conf
	(cond ((= cell (+ 2 (car (reverse-list curr-gen)))) nil)
		  ((eq cell-future t) (cons cell (life-array (+ 1 cell) curr-gen)))
		  (t (life-array (+ 1 cell) curr-gen)))))

;;; Simple append function
(defun append-list-to-list (list1 list2)
  (cond ((null list1) list2)
		(t (cond (car list1)
				 (append-list-to-list (cdr list1) list2)))))

;;; Simple reverse function
(defun reverse-list (list1)
  (cond ((null list1) nil)
		(t (append-list-to-list (reverse-list (cdr list1))(list (car list1))))))

;;; Main function
(defun lifeline (conf gen)
  (format t "~S~%" conf)
  (lifeline-helper conf gen))

;;; Recursive helper to main function
;;; TODO: Make closure of main function
(defun lifeline-helper (conf gen)
  (cond ((= gen 0) nil)
		((> gen 0) (format t "~S ~%" (life-array (- (car conf) 1) conf))
		 (lifeline-helper (life-array (- (car conf) 1) conf) (- gen 1)))))
