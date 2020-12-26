;;combination

(defun combination (n lists func)
  (labels ((f (list i)  
             (if (equal i 0) 
                 (apply func list) 
                 (mapcar #'(lambda (x) (f (append (list x) list) (1- i))) lists))\
)) 
    (f nil n))  
  nil)

;;swap

(defmacro swap (p q)       
  (let ((g (gensym)))      
    `(let ((,g ,q)) (setf ,q ,p ,p ,g))))

;;split

(defun split (str)
  (let ((p 0))
    (append
     (loop while (setq p (search " " str))
           collect (subseq str 0 p)
           do (setq str (subseq str (1+ p))))
     `(,(subseq str 0)))))

;;extgcd
(defun extgcd (a b)
  (if (= b 0)
      (list a 1 0)
      (let ((d (extgcd b (rem a b))))
        (list d (third d) (- (second d) (* (floor a b) (third d)))))))

;;euc
(defun euc (a m)
  (let ((d (extgcd a m)))
    (mod (second d) m)))
