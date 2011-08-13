(cl:in-package #:cl)

(cl:defpackage #:combinations
  (:use #:cl)
  (:export #:map-over-combinations
	   #:generate-combinations
	   #:with-combinations
	   #:sum-of-combinations))

(cl:in-package #:combinations)

(defvar *rattle-on* t
  "Enable printing while calculating")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-list (object)
    "Ensure object is a list (if it's an atom, put it in a list)"
    (cond ((and (not (null object)) (atom object)) (list object))
	  (t object))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-over-combinations (count list &optional (mapfunc #'(lambda (combination) 
								  (format *rattle-on* "~&~s " combination) 
								  nil)))
    "Map function over the combinations of K distinct objects chosen from a list of N elements" 
    (format *rattle-on* "~&Select combinations of ~s elements in list ~s~%" count list)
    (cond ((> count (length list))
	   (error "The specified number of elements for combination is bigger than the list size"))
	  ((= 0 count) 
	   (funcall mapfunc nil))	;  при нуле элементов в подмножестве - нет комбинаций.
	  ((eql count (length list)) 
	   (funcall mapfunc list))
	  (t
	   (let ((result nil) (mapfunc-result nil))
	     (dotimes (i count result) (push (1+ i) result)) ; первое подмножество
	     (setf result (reverse result))
	     (do ((number count))
		 ((< number 1) mapfunc-result) ;Return value is the last mapfunc's call
	       (setf mapfunc-result (funcall mapfunc
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

(defmacro with-combinations ((combination count-or-counts list) &body body)
  "Call body form in a bunch of combinations with a list of specified number of elements"
  (let ((ressym (gensym))
	(countsym (gensym)))
    `(let ((,ressym nil))  ;for eliminate compilation warning in sbcl
       (dolist (,countsym (ensure-list ,count-or-counts) ,ressym)  ;keep the action of return value as map-over-combinations, the last call of map-over-combinations
	 (setq ,ressym
	       (map-over-combinations ,countsym ,list (lambda (,combination)
							,@body))))
       ,ressym)))

(defun generate-combinations (count-or-counts list)
  "Create a list of combinations with specified number of elements"
  (let ((combinations nil))
    (with-combinations (combination count-or-counts list)
      (push combination combinations))
    (nreverse combinations)))

(defun sum-of-combinations (count-or-counts list &key (expected-sums nil) (collect-criteria #'(lambda (sum-of-combination expected-sums-with-combinations) 
												(declare (ignorable sum-of-combination expected-sums-with-combinations))
												t)))
  "Calculate sum of subset for subsets with specified number of elements"
  (let* ((list list)
	 (sum-of-list (reduce #'+ list))
	 (expected-sums (ensure-list expected-sums))
	 (sums-of-combinations nil)
	 (expected-sums-with-combinations nil))
     (format *rattle-on* "List are ~S~%Sum of list are ~d~%" list sum-of-list)
     (with-combinations (combination count-or-counts list)
       (let* ((sum-of-combination (round (reduce #'+ combination))))
	 (pushnew sum-of-combination sums-of-combinations)
	 (cond ((member sum-of-combination expected-sums)
		(if (funcall collect-criteria sum-of-combination expected-sums-with-combinations) ;when nil, do not collect this sum of combination
		    (pushnew (cons sum-of-combination (list combination)) expected-sums-with-combinations)))
	       (t nil))))
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
