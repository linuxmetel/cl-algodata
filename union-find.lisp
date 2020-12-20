;;Union Find

(defpackage union-find
  (:use :cl)
  (:nicknames :uf))

(in-package :union-find)

(defclass union-find ()
    ((vec :accessor union-find-vec
          :initform (make-array 2)
          :initarg :vec)))

(defun new (x)
  (make-instance 'union-find :vec (make-array x :initial-element -1)))

(defmethod root ((uf union-find) (x number))
  (if (> 0 (aref (union-find-vec uf) x))
      x
      (setf (aref (union-find-vec uf) x) (root uf (aref (union-find-vec uf) x)))))

(defmethod unite ((uf union-find) (x number) (y number))
  (let ((xx (root uf x))
        (yy (root uf y)))
    (unless (equal xx yy)
      (if (>= (aref (union-find-vec uf) xx) (aref (union-find-vec uf) yy))
          (setf (aref (union-find-vec uf) yy) xx)
          (setf (aref (union-find-vec uf) xx) yy)))))

(defmethod same ((uf union-find) (x number) (y number))
  (equal (root uf x) (root uf y)))

(in-package :cl-user)
