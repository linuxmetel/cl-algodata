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
    `(let ((,g ,q)) (setq ,q ,p ,p ,g))))

;;split

(defun split (str &key (by " "))
  (loop for last = 0 then (1+ p)
        for p = (search by str :start2 last)
        collect (subseq str last p)
        while p)
