(defpackage :bees
    (:use #:common-lisp)
  (:export
   :bee-stage
   :bee-search))

(in-package :bees)

(declaim (inline sort-seq random-value best-elems explore-point))

(defun square (x) (* x x))
(defun sort-seq (func seq) (sort seq #'< :key func))
(defun random-value (max-value) (- max-value (* 2 (random max-value))))
(defun best-elems (func seq max-number) (subseq (sort-seq func seq) 0 max-number))
(defmacro dec (x dec)
  `(setf ,x (* ,x ,dec)))

(defmacro collect (times &body body)
  `(let ((res nil))
     (dotimes (i ,times)
       (push ,@body res))
     res))

(defun explore-point (point interval quantity)
  (collect quantity
    (map 'vector #'(lambda (c) (+ (random-value interval) c))
	 point)))

(defun random-search (number-of-scouts null-point random-search-interval)
  (explore-point null-point random-search-interval number-of-scouts))

(defun min-elem (func seq)
  (let ((min-elem (elt seq 0))
	(min-elem-val (funcall func (elt seq 0))))
    (dotimes (i (length seq))
      (let* ((ith-elt (elt seq i))
	     (ith-elt-val (funcall func ith-elt)))
	(when (< ith-elt-val min-elem-val)
	  (setq min-elem ith-elt)
	  (setq min-elem-val ith-elt-val))))
    min-elem))

(defun merge-results (func random-res informed-res)
  (append random-res
	  (mapcar #'(lambda (l) (min-elem func l))
		  informed-res)))

(defun informed-search (points number-of-points interval)
  (mapcar #'(lambda (p) (explore-point p interval number-of-points)) points))

(defun bee-cycle (func null-point points random-search-interval random-point-number 
		  interval number-of-points chosen-number)
  (best-elems func
	      (merge-results func
			     (random-search random-point-number null-point random-search-interval)
			     (informed-search points number-of-points interval))
	      chosen-number))

(defun bee-stage (func null-point random-search-interval random-point-number 
		  interval number-of-points chosen-number iter eps)
  (let ((points (list null-point))
	(cur-val 0) (min-val 0))
    (dotimes (i iter)
      (print i)
      (setq cur-val
	    (funcall func
		     (first (setq points
				  (bee-cycle func (first points) points random-search-interval
					     random-point-number interval number-of-points chosen-number)))))
      (if (and (> (abs (- cur-val min-val)) eps))
	       (< cur-val min-val))
	  (setq min-val cur-val)
	  (return (first points))))
    (first points)))

(defun bee-search (func null-point random-search-interval random-point-number
		   interval number-of-points chosen-number iter eps decrease)
  (let ((cur-eps (* decrease interval)))
    (loop
      (setq null-point (bee-stage func null-point (dec random-search-interval decrease)
			   random-point-number (dec interval decrease) number-of-points chosen-number
			   iter (dec cur-eps decrease)))
      (when (< cur-eps eps)
	(return null-point)))))

;; test
;; finding argminimum of function 'peaks'
;; picture is avaliable here: 
;; http://www.mathworks.com/cmsimages/40280_wl_mn_ajaxcontrols_wl_18811.gif
;; the argminimum should be #(0.22833 -1.62550), result - -6.55113
;; 
;; (defun peaks (x y)
;;   (- (* 3
;; 	(expt (- 1 x) 2)
;; 	(exp (- ( -(expt x 2))
;; 		(expt (+ y 1) 2))))
;;      (* 10
;; 	(- (/ x 5)
;; 	   (expt x 3)
;; 	   (expt y 5))
;; 	(exp (- (- (expt x 2))
;; 		(expt y 2))))
;;      (* (/ 1 3)
;; 	(exp (- (- (expt (+ x 1) 2))
;; 		(expt y 2))))))
;; 
;; (defun wrap-peaks (vec)
;;   (apply #'peaks (coerce vec 'list)))
;;
;; (wrap-peaks (print (bee-search #'wrap-peaks #(0.0 0.0) 3.0 50 0.1 5 3 10 1.e-5 0.95)))
