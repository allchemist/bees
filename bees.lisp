(defpackage :bees
    (:use :common-lisp)
  (:export :bee-search :griewank-fn))

(in-package :bees)

;; вспомогательные функции, на всякий случай

(defun square (x) (* x x))
(defun random-value (max-val)
  (- max-val (* (random max-val) 2)))

;; random-point-neighbour:
;; случайная точка в окрестности точки point
;; удаление каждой координаты от исходной не более, чем range

(defun random-point-neighbour (point range)
  (let ((new-point (make-array (length point))))
    (dotimes (i (length point))
      (setf (aref new-point i)
	    (+ (aref point i)
	       (random-value range))))
    new-point))

;; explore-point:
;; density случайных точек в окрестности range точки point,
;; включая исходную точку

(defun explore-point (point range density)
  (let ((points (make-list (1+ density))))
    (dotimes (i density)
      (setf (elt points i)
	    (random-point-neighbour point range)))
    (setf (elt points density) point)
    points))

;; min-elem:
;; элемент последовательности seq,
;; значение функции func для которого минимально

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

;; sort-seq:
;; сортировка последовательности seq в порядке возрастания
;; в соответствии со значением функции func

(defun sort-seq (func seq) (sort seq #'< :key func))

;; best-elems:
;; max-number наилучших элементов последовательности seq
;; с точки зрения значения функции func
;; если последовательность не больше, чем предложенная длина
;; подпоследовательности (max-number), возвращается вся последовательность

(defun best-elems (func seq max-number)
  (if (> (length seq) max-number)
      (subseq (sort-seq func seq) 0 max-number)
      seq))

;; random-search:
;; случайный поиск
;; global-range - диапазон поиска, вектор #(min max)
;; global-density - кол-во случайных точек размерности dim
;; возвращается список точек, равномерно распределенных в заданном интервале

(defun random-search (global-range global-density dim)
  (let ((points (make-list global-density)))
    (dotimes (i global-density)
      (setf (elt points i)
	     (let ((point (make-array dim)))
	       (dotimes (i dim)
		 (setf (aref point i)
		       (+ (random (- (elt global-range 1)
				     (elt global-range 0)))
			  (elt global-range 0))))
	       point)))
    points))

;; informed-search:
;; поиск по окрестностям хороших точек
;; в окрестности range каждой точки из списка points
;; случайно распределяются density точек.
;; в каждой окрестности выбирается лучшая точка
;; и сохраняется в список new-points
;; если несколько точек имеют одинаковые значения функции,
;; то, вероятно, эти экстремумы эквивалентны,
;; и все повторяющиеся, кроме одной, удаляются. 

(defun informed-search (points func range density)
  (let ((new-points nil)
	(inc (/ density (length points))))
    (dotimes (i (length points))
      (push (min-elem func (explore-point (elt points i) range (round (incf density (* inc i)))))
	    new-points))
    (remove-duplicates (nreverse new-points) :test #'= :key func)))

;; bee-cycle:
;; основной цикл.
;; points - лучшие точки, полученные на предыдущем цикле
;; global-range, global-density - параметры для random-search
;; range, density - параметры для informed-search
;; chosen-number - кол-во отбираемых хороших точек
;; функция возвращает лучшие точки, которые будут использованы
;; в последующем вызове bee-cycle

(defun bee-cycle (func points global-range global-density range density chosen-number)
  (best-elems func
	      (append
	       (random-search global-range global-density (length (first points)))
	       (informed-search points func range density))
	      chosen-number))

;; vec-max-diff:
;; вспомогательная функция для оценки покоординатно максимального расстояния
;; между соответствующими координатами векторов

(defun vec-max-diff (v1 v2)
  (apply #'min (map 'list #'(lambda (x1 x2) (abs (- x1 x2))) v2 v1)))

;; bee-search:
;; основная функция,
;; iter - ограничение сверху по количеству итераций
;; init - один из вариантов:
;; * целое число - размерность точки в центре интервала global-range
;; * вектор - исходная популяция состоит из единственного вектора init
;; * список - исходная популяция - список векторов init

(defun bee-search (func init global-range global-density range density chosen-number iter)
  (let* ((old-points
	  (typecase init
	    (number
	       (list (make-array init
				 :initial-element (- (elt global-range 1)
						     (elt global-range 0)))))
	    (vector (list init))
	    (list init)))
	 (new-points (bee-cycle func old-points global-range global-density range density chosen-number)))
;; mod-params:
;; процедура модицикации параметров алгоритма в процессе работы
;; range: если улучшение меньше, чем размер окрестности, уменьшаем окрестность
;; global-density: интенсивность случайного поиска уменьшается с приближением к вероятному минимуму
    (flet ((mod-params (i)
	     (when (> range (vec-max-diff (first old-points) (first new-points)))
	       (setf range (/ range 1.2)))
	     (decf global-density (round (* global-density (square (/ (1+ i) iter))))))
;; print-stats:
;; печать текущих значений параметров алгоритма, которые могут быть изменены
	   (print-stats (i)
	     (format t "~A: best: ~A, range: ~A, global-density: ~A~%"
		     i (funcall func (first new-points)) range global-density)))
      (dotimes (i (1- iter))
;; пытаемся модифицировать параметры алгоритма каждые (round (/ iter 10)) раз
;; 10 модификаций за время выполнения
;; перед каждой модификацией печатаем состояние
	(when (zerop (mod i (round (/ iter 10))))
	  (print-stats (1+ i))
	  (mod-params i))
;; сохраняем старый набор лучших точек для сравнения с новым
	(setf old-points new-points)
;; получаем новый набор из старого с текущими параметрами алгоритма
	(setf new-points
	      (bee-cycle func old-points global-range global-density range density chosen-number)))
;; в итоге возвращаем лучшую точку
      (first new-points))))



;; тестовые функции

;; функция Гриванка:
;; минимум -10 достигается на нулевом векторе #(0 0 0 ... 0)
(defun griewank-fn (x)
  (let ((sum 0)
	(prod 1))
    (loop for i from 1 to (length x)
	  do (incf sum (square (elt x (1- i))))
	  do (setf prod (* prod (cos (/ (elt x (1- i)) (sqrt i))))))
    (- (/ (+ (/ sum 4000) (- prod) 1.1)))))
