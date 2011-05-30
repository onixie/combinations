(cl:in-package #:cl)
(require 'alexandria)

(cl:defpackage #:myfavor
  (:use #:cl #:alexandria)
  (:export #:generate-combination
	   #:random-permutation
	   #:lotto
	   #:random-select
	   #:make-range-integers))

(cl:in-package #:myfavor)

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

;; Число сочетаний из n элементов по m
(defun cmn (m n)
  (/ (factorial n)
     (* (factorial (- n m)) (factorial m)))) 

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

(defun solve-it ()
  (let* ((weights (myfavor:lotto 30 25))
	 (select-counts (list 1 2 3))
	 (selects (mapcan #'(lambda (count)
			      (myfavor:generate-combination count weights))
			  select-counts))
	 (weight-sum (apply '+ weights))
	 (select-sum-set (remove-duplicates (mapcar #'(lambda (select)
							(apply '+ select))
						    selects))))
    (values weights weight-sum selects select-sum-set (length select-sum-set))))