(defun htree-node (htree)
  (cond 
    ((equal 1 (length htree)) (first htree))
    (t htree)))

(defun htree-less (htree1 htree2)
  (< (htree-weight htree1) (htree-weight htree2)))

(defun htree-symbols (htree)
  (cond
    ((equal 1 (length(first(htree-node htree)))) (first(htree-node htree)))
    (t (first(first(htree-node htree))))))

(defun htree-weight (htree)
  (second(first (htree-node  htree))))

(defun root (htree)
  (first (htree-node htree)))

(defun htree-sort (htrees)
  (sort htrees #'> :key'cadar))

(defun make-huffman-tree (message)
  (make-huffman-tree-grow (make-frequency-list-encapsule (make-frequency-list message))))

(defun make-huffman-tree-grow (htree)
  (cond 
    ((< 1 (length htree))
     (make-huffman-tree-grow
      (sort
       (append
	(cons (htree-merge (first htree) (second htree)) nil)
	(rest(rest htree)))
       #'< :key'cadar)))
    (t htree)))

(defun make-frequency-list-encapsule (flist)
  (cond 
    ((equal 1 (length flist)) (cons flist nil))
    (t 
     (append      
      (cons (cons(first flist) nil) nil)
      (make-frequency-list-encapsule (rest flist))))))

(defun make-frequency-list (message)
  (sort (make-frequency-list-recursive message nil) #'< :key'cadr))

(defun make-frequency-list-recursive (message freqlist)
 (cond ((endp message) freqlist)
       (t (make-frequency-list-recursive 
	   (rest message)
	   (make-frequency-list-node-update (first message) freqlist)))))

(defun make-frequency-list-node-update (messagenode freqlist)
  (cond ((endp freqlist) (make-frequency-list-node-new messagenode))
	((messagenode-match-freqlistnode messagenode (first(first freqlist)))
	 (make-frequency-list-node-increment freqlist))
	(t (cons 
	    (first freqlist)
	    (make-frequency-list-node-update messagenode (rest freqlist))))))

(defun messagenode-match-freqlistnode (messagenode freqlistnode)
  (equal messagenode (first freqlistnode)))

(defun make-frequency-list-node-new (messagenode)
  (list(list(list messagenode) 1)))

(defun make-frequency-list-node-increment (freqlist)
  (cons 
   (list (first(first freqlist))
	 (1+ (second(first freqlist))))
   (rest freqlist)))

(defun htree-merge (htree1 htree2)
  (cons
   (list
    (append
     (htree-symbols htree1)
     (htree-symbols htree2))
    (+
     (cond 
       ((equal 1 (length(first(htree-node  htree1)))) (second(htree-node htree1)))
       (t (second(first(htree-node htree1)))))
     (cond 
       ((equal 1 (length(first(htree-node htree2)))) (second(htree-node htree2)))
       (t (second(first(htree-node htree2)))))))
   (append 
    (list htree1) 
    (list htree2))))

(defun leaf-p (htree)
  (= 1 (length (first(htree-node htree)))))

(defun left-subhtree (htree)
  (cond
    ((equal 1 (length(second(htree-node htree)))) (second(htree-node htree)))
    (t (second(htree-node htree)))))

(defun right-subhtree (htree)
  (cond
    ((equal 1 (length(second(htree-node htree)))) (second(rest(htree-node htree))))
    (t (second(rest(htree-node htree))))))

(defun encode (message huffman-tree)
  (cond
    ((endp message) nil)
    (t 
     (concatenate 'string (encode-single (first message) huffman-tree)
		  (cond
		    ((equal 1 (length message)) nil)
		    (t (encode (rest message) huffman-tree)))))))

(defun encode-single (message huffman-tree)
  (cond
    ((leaf-p huffman-tree) nil)
    ((message-in-list message (htree-symbols (left-subhtree huffman-tree))) 
     (concatenate 'string '(#\1) (encode-single message (left-subhtree huffman-tree))))
    ((message-in-list message (htree-symbols (right-subhtree huffman-tree))) 
     (concatenate 'string '(#\0) (encode-single message (right-subhtree huffman-tree))))))

(defun message-in-list (message list)
  (cond
    ((equal nil list) nil)
    ((equal message (first list)) t)
    (t (message-in-list message (rest list)))))

(defun decode (binary huffman-tree)
  (decode-binary binary huffman-tree huffman-tree))

(defun decode-binary (binary current-huffman-tree full-huffman-tree)
  (cond 
    ((leaf-p current-huffman-tree) 
     (append 
      (list(first(htree-symbols current-huffman-tree)))
      (decode-binary binary full-huffman-tree full-huffman-tree)))
    ((>= 0 (length binary)) nil)
    ((string= "1" (subseq binary 0 1))
     (decode-binary (subseq binary 1) (left-subhtree current-huffman-tree) full-huffman-tree))
    ((string= "0" (subseq binary 0 1))
     (decode-binary (subseq binary 1) (right-subhtree current-huffman-tree) full-huffman-tree))))

