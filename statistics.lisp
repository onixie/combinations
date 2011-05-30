(cl:in-package #:cl)

(cl:defpackage #:omnistat
  (:use #:cl)
  (:export #:generate-combination
	   #:map-over-combination
	   #:with-combinations
	   #:random-permutation
	   #:lotto
	   #:random-select
	   #:make-range-integers))

(cl:in-package #:omnistat)

;;; МАТЕМАТИЧЕСКИЕ МЕТОДЫ В ИНФОРМАТИКЕ
;;  Генерирование всех number-элементных подмножеств множества (1, ... ,n).
(defun generate-combination (count list)
  "Generate the combinations of K distinct objects chosen from the N elements of a list"
  (cond ((> count (length list))
	 (error "длина генерируемого списка не может быть больше начального списка!"))
	((= 0 count) (list nil))	;  при нуле элементов в подмножестве - нет комбинаций.
	((eql count (length list)) (list list))
	(t
	 (let ((result nil) (combinations nil))
	   (dotimes (i count result) (push (1+ i) result)) ; первое подмножество
	   (setf result (reverse result))
	   (do ((number count))
	       ((< number 1) (reverse combinations))
	     (push 
	      (mapcar (lambda (item) (elt list (1- item))) (copy-list result))
	      combinations)
	     (if (eql (elt result (1- count)) (length list))
		 (setf number (1- number))
		 (setf number count))
	     (when (>= number 1) 
	       (do ((i count (1- i)))
		   ((< i number) nil) 
		 (setf (elt result (1- i)) (- (+ (elt result (1- number)) i 1) number)))))))))

(defun map-over-combination (count list &optional (mapfunc #'(lambda (combination) (print combination) nil)))
  "Map function over the combinations of K distinct objects chosen from the N elements of a list"
  (cond ((> count (length list))
	 (error "the count specified is bigger than the list size"))
	((= 0 count) (funcall mapfunc nil))	;  при нуле элементов в подмножестве - нет комбинаций.
	((eql count (length list)) (funcall mapfunc list))
	(t
	 (let ((result nil) (mapfunc-result nil))
	   (dotimes (i count result) (push (1+ i) result)) ; первое подмножество
	   (setf result (reverse result))
	   (do ((number count))
	       ((< number 1) mapfunc-result)
	     (setq mapfunc-result 
		   (funcall mapfunc
			    (mapcar (lambda (item) 
				      (elt list (1- item))) 
				    (copy-list result))))
	     (if (eql (elt result (1- count)) (length list))
		 (setf number (1- number))
		 (setf number count))
	     (when (>= number 1) 
	       (do ((i count (1- i)))
		   ((< i number) nil) 
		 (setf (elt result (1- i)) (- (+ (elt result (1- number)) i 1) number)))))))))

(defmacro with-combinations ((combination count-or-counts list) &body body)
  (cond ((null count-or-counts) nil)
	((and (atom count-or-counts) (integerp count-or-counts))
	 `(map-over-combination ,count-or-counts ,list (lambda (,combination)
							 ,@body)))
	((consp count-or-counts)
	 `(append (with-combinations (,combination ,(car count-or-counts) ,list)
		    ,@body)
		  (with-combinations (,combination ,(cdr count-or-counts) ,list)
		    ,@body)))
	(t nil)))

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

(defmacro omnifactor (&key (weight-count 30) (max-weight 25) (default-value-of-expected '(1 2 3)) (default-count-selects '(1 2 3)))
  `(let* ((weights (omnistat:lotto ,weight-count ,max-weight))
	  (sum-of-weights (reduce #'+ weights))
	  (value-of-expected (list ,@default-value-of-expected))
	  (all-of-result nil)
	  (expected-result nil))
     (format t "Weights are ~S~&Sum of weights are ~d~&" weights sum-of-weights)
     (omnistat::with-combinations (combination ,default-count-selects weights)
       (let* ((sum-of-combination (reduce #'+ combination))
	      (value-of-result (round (* (/ sum-of-combination sum-of-weights) 100))))
	 (format t "Combination are ~S~&Sum of combination are ~d~&" combination sum-of-combination)
	 (format t "Value (sum-of-combination/sum-of-weight*100) are ~d~&" value-of-result)
	 (pushnew value-of-result all-of-result)
	 (cond ((member value-of-result value-of-expected) (pushnew (cons value-of-result (list combination)) expected-result))
	       (t nil))))
     (values sum-of-weights (sort all-of-result #'<) (sort expected-result #'< :key #'car))))