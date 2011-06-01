(cl:in-package #:cl)

(cl:defpackage #:omnistat
  (:use #:cl)
  (:export #:generate-combinations
	   #:map-over-combinations
	   #:with-combinations
	   #:random-permutation
	   #:lotto
	   #:random-select
	   #:make-range-integers))

(cl:in-package #:omnistat)

;;; МАТЕМАТИЧЕСКИЕ МЕТОДЫ В ИНФОРМАТИКЕ
;;  Генерирование всех number-элементных подмножеств множества (1, ... ,n).
;; (defun generate-combination (count list)
;;   "Generate the combinations of K distinct objects chosen from the N elements of a list"
;;   (cond ((> count (length list))
;; 	 (error "длина генерируемого списка не может быть больше начального списка!"))
;; 	((= 0 count) (list nil))	;  при нуле элементов в подмножестве - нет комбинаций.
;; 	((eql count (length list)) (list list))
;; 	(t
;; 	 (let ((result nil) (combinations nil))
;; 	   (dotimes (i count result) (push (1+ i) result)) ; первое подмножество
;; 	   (setf result (reverse result))
;; 	   (do ((number count))
;; 	       ((< number 1) (reverse combinations))
;; 	     (push 
;; 	      (mapcar (lambda (item) (elt list (1- item))) (copy-list result))
;; 	      combinations)
;; 	     (if (eql (elt result (1- count)) (length list))
;; 		 (setf number (1- number))
;; 		 (setf number count))
;; 	     (when (>= number 1) 
;; 	       (do ((i count (1- i)))
;; 		   ((< i number) nil) 
;; 		 (setf (elt result (1- i)) (- (+ (elt result (1- number)) i 1) number)))))))))
(defvar *verbose* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-over-combinations (count list &optional (mapfunc #'(lambda (combination) (format *verbose* "~&~s " combination) nil)))
    "Map function over the combinations of K distinct objects chosen from the N elements of a list"
    (format *verbose* "~&Specified count ~s with list ~s~%" count list)
    (cond ((> count (length list))
	   (error "The count specified is bigger than the list size"))
	  ((= 0 count) 
	   (funcall mapfunc nil))	;  при нуле элементов в подмножестве - нет комбинаций.
	  ((eql count (length list)) 
	   (funcall mapfunc list))
	  (t
	   (let ((result nil) (mapfunc-result nil))
	     (dotimes (i count result) (push (1+ i) result)) ; первое подмножество
	     (setf result (reverse result))
	     (do ((number count))
		 ((< number 1) mapfunc-result)
	       (setq mapfunc-result (funcall mapfunc
					     (mapcar (lambda (item) 
						       (elt list (1- item)))
						     (copy-list result))))
	       (if (eql (elt result (1- count)) (length list))
		   (setf number (1- number))
		   (setf number count))
	       (when (>= number 1) 
		 (do ((i count (1- i)))
		     ((< i number) nil) 
		   (setf (elt result (1- i)) (- (+ (elt result (1- number)) i 1) number))))))))))

;;Because i eval the args in macro expansion time, it may have problem when invoked with runtime objects as args.
;; Is there a way to prevent macro expansion until execution time??
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-combinations ((combination count-or-counts list) &body body)
    ""
    (let ((list `(list ,@(eval list)))
	  (count-or-counts (eval count-or-counts)))
      (cond ((null count-or-counts) nil)
	    ((and (atom count-or-counts) (integerp count-or-counts))
	     `(map-over-combinations ,count-or-counts ,list (lambda (,combination)
							      ,@body)))
	    ((consp count-or-counts)
	     `(append (with-combinations (,combination (car (list ,@count-or-counts)) ,list)
			,@body)
		      (with-combinations (,combination (cdr (list ,@count-or-counts)) ,list)
			,@body)))
	    (t nil)))))

(defun generate-combinations (count list)
  "Create a list of combinations"
  (let ((combinations nil))
    (map-over-combinations count list #'(lambda (combination)
					  (push combination combinations)))))

(defun sum-of-combinations (count-or-counts list &key (expected-sums nil) (collect-criteria #'(lambda (sum-of-combination expected-sums-with-combinations) t)))
  "Calculate sum subset for subsets with specified number of elements"
  (let* ((list list)
	 (sum-of-list (reduce #'+ list))
	 (expected-sums (cond ((atom expected-sums) (list expected-sums))
			      (t expected-sums)))
	 (sums-of-combinations nil)
	 (expected-sums-with-combinations nil))
     (format *verbose* "List are ~S~%Sum of list are ~d~%" list sum-of-list)
     (mapcar #'(lambda (count) 
		 (map-over-combinations count list
					#'(lambda (combination)
					    (let* ((sum-of-combination (round (reduce #'+ combination))))
					      (pushnew sum-of-combination sums-of-combinations)
					      (cond ((member sum-of-combination expected-sums)
						     (if (funcall collect-criteria sum-of-combination expected-sums-with-combinations)
							 (pushnew (cons sum-of-combination (list combination)) expected-sums-with-combinations)))
						    (t nil))))))
	     (cond ((atom count-or-counts) (list expected-sums))
		   (t count-or-counts)))
     (values (sort expected-sums-with-combinations #'< :key #'car) (sort sums-of-combinations #'<))))

;; Число сочетаний из n элементов по m
;; (defun cmn (m n)
;;   (/ (factorial n)
;;      (* (factorial (- n m)) (factorial m)))) 

;; Перетасовать список в случайном порядке
(defun random-permutation (list)
  "Generate a random permutation of the elements of a list."
  (if (null list)
      list
      (do ((result-list nil))
	  ((equal (length list) (length result-list)) result-list) ; условие окончания цикла
	(push (nth (random (length list)) list) result-list)
	(setf result-list (delete-duplicates result-list))))) ; разрушающая операция

;; Создать список из списка. В новый список включить н-ое количество элементов,
;; выбрав их случайным образом из начального списка.
(defun random-select (list random) 
  "Extract a given number of randomly selected elements from a list. 
     The selected items shall be returned in a list."
  (if (null random) 
      nil
      (do ((rang (length list))
	   (result nil))
	  ((= 0 random) result)
	(setf result (cons (elt list (random rang)) result))
	(setf random (1- random)))))

;; Создать список содержащий все целые числа в заданном диапазоне
(defun make-range-integers (begin end)
  "Create a list containing all integers within a given range"
  (do ((list nil))
      ((= begin end) (reverse (push begin list)))
    (push begin list)
    (setf begin (1+ begin))))

;; Вариант с функцией отображения
(defun make-range-integers. (begin end)
  (let ((list nil)
	(index (1- begin)))
    (declare (ignorable list))
    (mapcar (lambda (item) 
	      (setf index (1+ index)) item index) 
	    (make-list (- (1+ end) begin)))))

;; Создать список из случайно выбранных чисел в количестве заданном первым аргументом. 
;; Числа сгенерить в диапазоне от 1 до заданного вторым аргументом 
(defun lotto (count range)
  "Draw N different random numbers from the set 1..M."
  (random-select		; Создать список из списка.
   (make-range-integers 1 range) count))	; Создать список содержащий все целые числа в заданном диапазоне

;; This one will cause heap exhausted, we'd better find a functional way to handle it.
;; (defun reverse-back (&key (weight-count 30) (max-weight 25) (default-select-counts (list 1 2 3 4)))
;;   (let* ((weights (lotto weight-count max-weight))
;; 	 (select-counts default-select-counts)
;; 	 (weight-sum (apply '+ weights))
;; 	 (select-sum-set (sort (remove-duplicates (mapcar #'(lambda (select)
;; 							(round (* (/ (apply '+ select) weight-sum) 100)))
;; 						    (mapcan #'(lambda (count)
;; 								(generate-combination count weights)
;; 								)
;; 							    select-counts))) #'>)))
;;     (values weights weight-sum select-sum-set)))

;; (defmacro omnifactor (&key (weight-count 30) (max-weight 25) (default-value-of-expected '(1 2 3)) (default-count-selects '(1 2 3)))
;;   `(let* ((weights (omnistat:lotto ,weight-count ,max-weight))
;; 	  (sum-of-weights (reduce #'+ weights))
;; 	  (value-of-expected (list ,@default-value-of-expected))
;; 	  (all-of-result nil)
;; 	  (expected-result nil))
;;      (format t "Weights are ~S~&Sum of weights are ~d~&" weights sum-of-weights)
;;      (omnistat::with-combinations (combination ,default-count-selects weights)
;;        (let* ((sum-of-combination (reduce #'+ combination))
;; 	      (value-of-result (round (* (/ sum-of-combination sum-of-weights) 100))))
;; ;	 (format t "Combination are ~S~&Sum of combination are ~d~&" combination sum-of-combination)
;; ;	 (format t "Value (sum-of-combination/sum-of-weight*100) are ~d~&" value-of-result)
;; 	 (pushnew value-of-result all-of-result)
;; 	 (cond ((member value-of-result value-of-expected)
;; 		(prog1 (pushnew (cons value-of-result (list combination)) expected-result)
;; 		  (if (>= (count value-of-result expected-result :key #'car) 2)
		      
;; 		      (print expected-result))))
;; 	       (t nil))))
;;      (values sum-of-weights (sort all-of-result #'<) (sort expected-result #'< :key #'car))))

;; (defmacro omnifactorv2 (&key (default-weights '(1 2 3)) (default-value-of-expected '(1 2 3)) (default-count-selects '(1 2 3)) (default-result-count 5))
;;   `(let* ((weights (list ,@default-weights))
;; 	  (sum-of-weights (reduce #'+ weights))
;; 	  (value-of-expected (list ,@default-value-of-expected))
;; 	  (all-of-result nil)
;; 	  (expected-result nil)
;; 	  (result-count ,default-result-count))
;;      (format t "Weights are ~S~&Sum of weights are ~d~&" weights sum-of-weights)
;;      (omnistat::with-combinations (combination ,default-count-selects weights)
;;        (let* ((sum-of-combination (reduce #'+ combination))
;; 	      (value-of-result (round sum-of-combination)))
;; ;	 (format t "Combination are ~S~&Sum of combination are ~d~&" combination sum-of-combination)
;; ;	 (format t "Value (sum-of-combination/sum-of-weight*100) are ~d~&" value-of-result)
;; 	 (pushnew value-of-result all-of-result)
;; 	 (cond ((member value-of-result value-of-expected)
;; 		(progn 
;; 		  (if (< (count value-of-result expected-result :key #'car) result-count)
;; 		      (pushnew (cons value-of-result (list combination)) expected-result)
;; 					;(print expected-result)
;; 		      )
;; 		  expected-result))
;; 	       (t nil))))
;;      (values sum-of-weights (sort all-of-result #'<) (sort expected-result #'< :key #'car))))


;; (omnistat::omnifactorv2 :default-weights (6.94 1.98 2.54 2.33 5.38 1.72 2.04 3.76 6.09 6.19 3.9 0.57 9.65 2.45 6.78 4.55 6.99 4.16 3.58 1.36 2.73 1.19 1.47 2.15 2.72 4.51 1.86 0.4 1.86 0.4)
;; 				 :default-value-of-expected (48 73 79 54 64 60 29 70 53 42 64 59 40 49 57 63 44 61 72 62 62 47 42 46 52 37 54 57 69 37 46 59 38 56 53 33 53 58 41 56 63 59 60 32 72 41 46 73 61 51 53 43 47 55 34 46 76 66 25 73 47 50 63 45 52 50 50 57 46 63 63 57 33 60 38 43 35 48 47 42 53 52 60 44 40 45 54 51 51 59 50 74 70 45 24 43 75 68 36 32 16 34 39 39 49 57 46 72 33 59 60 48 68 68 66 49 57 56 48 57 47)
;; 				 :default-count-selects (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
;; 				 )


;; (sort (remove-duplicates (list 48 73 79 54 64 60 29 70 53 42 64 59 40 49 57 63 44 61 72 62 62 47 42 46 52 37 54 57 69 37 46 59 38 56 53 33 53 58 41 56 63 59 60 32 72 41 46 73 61 51 53 43 47 55 34 46 76 66 25 73 47 50 63 45 52 50 50 57 46 63 63 57 33 60 38 43 35 48 47 42 53 52 60 44 40 45 54 51 51 59 50 74 70 45 24 43 75 68 36 32 16 34 39 39 49 57 46 72 33 59 60 48 68 68 66 49 57 56 48 57 47)) #'<)

;; (sort (list 48 73 79 54 64 60 29 70 53 42 64 59 40 49 57 63 44 61 72 62 62 47 42 46 52 37 54 57 69 37 46 59 38 56 53 33 53 58 41 56 63 59 60 32 72 41 46 73 61 51 53 43 47 55 34 46 76 66 25 73 47 50 63 45 52 50 50 57 46 63 63 57 33 60 38 43 35 48 47 42 53 52 60 44 40 45 54 51 51 59 50 74 70 45 24 43 75 68 36 32 16 34 39 39 49 57 46 72 33 59 60 48 68 68 66 49 57 56 48 57 47) #'<)

;; (omnistat::omnifactorv2 :default-weights (6.94 1.98 2.54 2.33 5.38 1.72 2.04 3.76 6.09 6.19 3.9 0.57 9.65 2.45 6.78 4.55 6.99 4.16 3.58 1.36 2.73 1.19 1.47 2.15 2.72 4.51 1.86 0.4 1.86 0.4)
;; 				 :default-value-of-expected (16 24 25 29 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 66 68 69 70 72 73 74 75 76 79)
;; 				 :default-count-selects (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))