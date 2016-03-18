(equal? (add-nums (list)) 0) 
(equal? (add-nums (list 4 2 'a 4)) 10) 


(equal? (length (list)) 0) 
(equal? (length (list 4 8)) 2) 


(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "negative index"))])
    (get-nth null -2))  
(equal? (get-nth (list 1 2 3) 2) 3)

(equal? (every-other (list 7 2 6 8 5)) (list 7 6 5)) 
(equal? (every-other (list 1 3)) (list 1))   
(equal? (every-other (list)) (list)) 


(equal? (map (lambda (x) (* x x)) (list))
     (list))      
(equal? (map (lambda (x) (+ x x)) (list 1 2 3))
     (list 2 4 6))       


(equal? (map2 (lambda (x y) (* x y)) (list 1 3 5) (list 2 4 6))
     (list 2 12 30))      
(equal? (map2 (lambda (x y) (* x y)) (list 1) (list 2 8 9))
     (list 2))      
(equal? (map2 (lambda (x y) (* x y)) (list) (list 1 4 7))
     (list))      


(equal? (filter (lambda (x) (= (modulo x 2) 1))
               (list 1 2 3 4 5 6 7 8))
     (list 1 3 5 7))      
(equal? (filter (lambda (x) (= (modulo x 2) 0))
               (list 1 2 3 4 5 6 7 8))
     (list 2 4 6 8))  
(equal? (filter (lambda (x) (= (modulo x 2) 1))
               (list))
     (list))      


(equal? (call-all (list (lambda () 4) (lambda () 6) (lambda () 8)))
     (list 4 6 8))        
(equal? (call-all (list))
     (list))   
