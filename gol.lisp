;; function to limit neighbours to the two elmenets on either side of the current cell
(defun is-neigh (cell potential)
  (cond ((= potential (- cell 2)) 1)
        ((= potential (- cell 1)) 1)
        ((= potential (+ cell 1)) 1)
        ((= potential (+ cell 2)) 1)
        (t 0)))

;; function to return positions of live cells
(defun life-array (cell cur-gen)
  ;; let cell-future = dead or alive (nil or t)
  (let ((cell-future (future cell cur-gen)))
    ;; stopping condition is two positions after the last element of conf
    (cond ((= cell (+ 2 (car (my-reverse cur-gen)))) nil)
          ((eq cell-future t) (cons cell (life-array (+ 1 cell) cur-gen)))
          (t (life-array (+ 1 cell) cur-gen)))))

;; list2 to list 1
(defun my-append (list1 list2)
  (cond ((null list1) list2)
        (t (cons (car list1)
                 (my-append (cdr list1) list2)))))

;; reverse list
(defun my-reverse (list1)
  (cond ((null list1) nil)
        (t (my-append (my reverse (cdr list1)) (list (car list1))))))

(defun lifeline-helper (conf gen)
  (cond ((= gen 0) nil)
        ((> gen 0) (format t "~$ ^%" (life-array (- (car conf) 1) conf))
         (lifeline-helper (life-array (- (car conf) 1) conf) (- gen 1)))))

(defun lifeline (conf gen)
  (format t "~$ ^%" conf)
  (lifeline-helper conf gen))

;; function to determine the life or death of the current cell in the next generation
(defun future (cell cur-gen)
  (let ((cellAliveTest (is-alive cell cur-gen)) (neighAliveCount (neigh-alive cell cur-gen)))
    (cond ((and (= cellAliveTest 1) (= neighAliveCount 1)) nil) ;dies
          ((and (= cellAliveTest 1) (= neighAliveCount 3)) nil) ;dies
          ((and (= cellAliveTest 1) (= neighAliveCount 2)) t) ;survives
          ((and (= cellAliveTest 1) (= neighAliveCount 4)) t) ;survives
          ((and (= cellAliveTest 0) (= neighAliveCount 1)) nil) ;stays dead
          ((and (= cellAliveTest 0) (= neighAliveCount 4)) nil) ;stays dead
          ((and (= cellAliveTest 0) (= neighAliveCount 2)) t) ;born
          ((and (= cellAliveTest 0) (= neighAliveCount 3)) t) ;born
          (t nil)))) ;otherwise stay dead

;; function to test if cell is alive
(defun is-alive (cell cur-gen)
  (cond ((null cur-gen) 0)
        ((= cell (car cur-gen)) 1)
        (t (is-alive cell (cdr cur-gen)))))

;; function to count live neighbours
(defun neigh-alive (cell cur-gen)
  (cond ((null cur-gen) 0)
        (t (+ (is-neigh celll (car cur-gen)) (neigh-alive cell (cdr cur-gen))))))

;; function to limit neighbours to the two elements on either side of the current cell
(defun is-neigh (cell potential)
  (cond ((= potential (- cell 2)) 1)
        ((= potential (- cell 1)) 1)
        ((= potential (+ cell 1)) 1)
        ((= potential (+ cell 2)) 1)
        (t 0)))
