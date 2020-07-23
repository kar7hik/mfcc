(in-package #:mfcc)

(defun range-helper (start stop step)
  (iter
    (for i :from start :to stop :by step)
    (collect i result-type 'vector)))


(defun range (start &key (stop nil stop-supplied-p) (step 1))
  "Creates an array for given start and stop conditions. default step value is 1. The resulting array will be the type of step."
  (if stop-supplied-p
      (range-helper start stop step)
      (range-helper 0 start step)))


(defun create-file-path (filename)
  "Creates a file path from the filename."
  (merge-pathnames filename
                   *current-directory*))

(defun create-data-file-path (filename)
  "Creates a file path from the filename."
  (merge-pathnames
   (merge-pathnames filename *data-directory*)
                   *current-directory*))


(defun coerce-sequence (seq conversion-type)
  (iter
    (for elt :in-sequence seq)
    (alexandria:coercef elt conversion-type)
    (collect elt result-type 'vector)))



(defun dot-product (a b)
  "Function returns the dot-product of the input sequences."
  ;; (declare (optimize (speed 3))
  ;;          (type (simple-array single-float) a b))
  (reduce #'+ (map 'simple-vector #'* a b)))




(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))


(defun get-row (arr)
  (car (array-dimensions arr)))

(defun get-col (arr)
  (cadr (array-dimensions arr)))


(defun coerce-to-matrix (x)
  (setf x (lazy-array x))
  (trivia:ematch (shape x)
    ((~)
     (reshape x (~ 1 ~ 1)))
    ((~l (list range))
     (reshape x (~ 1 (range-size range) ~ 1)))
    ((~l (list range-1 range-2))
     (reshape x (~ 1 (range-size range-1) ~ 1 (range-size range-2))))))


(defun transpose (x)
  (reshape
   (coerce-to-matrix x)
   (τ (m n) (n m))))



(defun transpose% (arr)
  (let* ((row (get-row arr))
         (col (get-col arr))
         (tran (make-array (list col row))))
    (iter
      (for i :from 0 :below row)
      (iter
        (for j :from 0 :below col)
        (setf (aref tran j i)
              (aref arr i j))))
    tran))


;; (defun matrix-multiplication (A B)
;;   ;; (declare (type (simple-array double-float *) A B))
;;   (let* ((m (car (array-dimensions A)))
;;          (n (cadr (array-dimensions A)))
;;          (l (cadr (array-dimensions B)))
;;          (out (make-array `(,m ,l) :initial-element 0d0
;;                           :element-type 'double-float)))
;;     (declare (type fixnum m n l))
;;     (iter
;;       (for i :from 0 :below m)
;;       (iter
;;         (for k :from 0 :below l)
;;         (setf (aref out i k)
;;               (iter
;;                 (for j :from 0 :below n)
;;                 (sum (* (aref A i j)
;;                         (aref B j k)))))))
;;     out))


(defun matrix-multiplication (A B)
  (setf lparallel:*kernel* (lparallel:make-kernel *num-worker-threads*))
  (let* ((m (car (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (l (cadr (array-dimensions B)))
         (out (make-array `(,m ,l) :initial-element 0d0
                          :element-type 'double-float)))
    (declare (type fixnum m n l))
    (pdotimes (i m)
      (dotimes (k l)
        (setf (aref out i k)
              (iter
                (for j :from 0 :below n)
                (sum (* (aref A i j)
                        (aref B j k)))))))
    out))



(defun save-2d-array-to-file (filename array)
  (with-open-file (fstream
                   (create-data-file-path filename)
                   :direction :output
                   :if-exists :supersede)
    (let ((rows (get-row array))
          (cols (get-col array)))
      (iter
        (for row :from 0 :below rows)
        (iter
          (for col :from 0 below cols)
          (format fstream "~10A" (aref array row col)))
        (format fstream "~&~%")))))






;; (declaim (ftype (function (matrix matrix &optional (or null fixnum) (or null fixnum) (or null fixnum)) matrix) matprod))
;; (defun matprod (fst snd &optional d1 d2 d3)
;;   (declare (optimize (speed 3) (debug 0) (space 0) (safety 0)))
;;   (if d1
;;      (let ((prod (make-array (list d1 d3) :element-type (array-element-type fst))))
;;         (loop :for i :from 0 :below d1 :do
;;           (loop :for j :from 0 :below d3 :do
;;             (setf (aref prod i j)
;;                   (loop :for k :from 0 :below d2
;;                         :sum (the integer (* (the integer (aref fst i k)) (the integer (aref snd k j))))))))
;;       prod)
;;      (destructuring-bind (m n) (array-dimensions fst)
;;        (destructuring-bind (n1 l) (array-dimensions snd)
;;          (assert (and (= n n1)
;;                       (eql (array-element-type fst) (array-element-type fst))))
;;          (let ((prod (make-array (list m l) :element-type (array-element-type fst))))
;;              (loop :for i :from 0 :below m :do
;;                (loop :for j :from 0 :below l :do
;;                  (setf (aref prod i j)
;;                        (loop :for k :from 0 :below n
;;                              :sum (the integer (* (the integer (aref fst i k)) (the integer (aref snd k j))))))))
;;            prod)))))



;; (time (matprod (list-to-2d-array (power-spectrum (audio-obj *mfcc-obj*)))
;;                (transpose (filter-banks *mfcc-obj*))))




(defun apply-log (seq &key (base 10) (multiplier 1))
  (iter
    (for elt :in-sequence seq)
    (collect (* multiplier
                (log elt base)))))



(defun product (x &optional axis)
  (compute (β* #'* 1 x axis)))


(defun sum (x &optional axis)
  (compute (β* #'+ 0 x axis)))


(defun 2d-mean (2d-array &key (axis 0) (type 'single-float))
  (let ((divisor (if (zerop axis)
                     (get-row 2d-array)
                     (get-col 2d-array))))
    (compute (alpha #'/ (sum 2d-array axis)
                    (coerce divisor type)))))
