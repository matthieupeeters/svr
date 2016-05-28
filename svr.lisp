(ql:quickload 'usocket)

;; Copied from Land of Lisp, by Conrad Barski

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
	       (coerce (list c1 c2) 'string)
	       :radix 16
	       :junk-allowed t)))
    (if code
	(code-char code)
	default)))

(defun decode-param (s)
  (labels ((f (lst)
	     (when lst
	       (case (car lst)
		 (#\% (cons (http-char (cadr lst) (caddr lst))
			    (f (cdddr lst))))
		 (#\+ (cons #\Space (f (cdr lst))))
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
	(i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
		      (+ 2 (position #\Space s))
		      (position #\Space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
	(cons url '()))))

(defun get-header (stream)
  (let* ((s (read-line stream nil ""))
	 (h (let ((i (position #\: s)))
	      (when i
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(parse-params content)))))

(defun serve (request-handler)
  (let ((socket (usocket:socket-listen "0.0.0.0" 80 :reuse-address t)))
    (unwind-protect
	 (loop (with-open-stream (stream (usocket:socket-stream (usocket:socket-accept socket)))
		 (let* ((url (parse-url (read-line stream nil)))
			(path (car url))
			(header (get-header stream))
			(params (append (cdr url)
					(get-content-params stream header)))
			(*standard-output* stream))
		   (funcall request-handler path header params))))
      (usocket:socket-close socket))))

(defmacro with-safe-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*read-eval* nil))
       ,@body)))

(defparameter *board* (make-array '(120 30) :initial-element nil))

(defun limit (v to)
  (cond ((< v 0) (+ v to))
	((>= v to) (- to v))
	(t v)))

(defun count-neighbors (board px py)
  (let ((tox (car (array-dimensions board)))
	(toy (cadr (array-dimensions board))))
    (-
     (loop for x from (1- px) to (1+ px)
	sum (loop for y from (1- py) to (1+ py)
	       sum (if (aref board
			     (limit x tox)
			     (limit y toy))
		       1 0)))
     (if (aref board px py)
	 1
	 0))))


(defun determine-cell (board px py)
  (let ((nb (count-neighbors board px py)))
    (if (aref board px py)
	(and (> nb 1) (< nb 4))
	(= nb 3))))



(defun randomize-cell (board px py)
  (declare (ignore board)
	   (ignore px)
	   (ignore py))
  (= 0 (random 2)))



(defun rebuild-board (board func)
  (let ((tox (car (array-dimensions board)))
	(toy (cadr (array-dimensions board))))
    (let ((rv (make-array
	       (list tox toy)
	       :initial-element nil)))
      (loop for x from 0 to (1- tox)
	 do (loop for y from 0 to (1- toy)
	       do (setf (aref rv x y) (funcall func board x y))))
      rv)))

(defun generation-board (board)
  (rebuild-board board #'determine-cell))

(defun randomize-board (board)
  (rebuild-board board #'randomize-cell))


(defun run-board (steps)
  (dotimes (step steps)  (step-board)))


(defun generate-html (board)
  (let ((html (make-array '(0) :element-type 'character
			  :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s html)
            (format s "<html><head></head><body>
                 <a href=\"randomize\">randomize</a><br/>
                 <a href=\"step\">step</a><br/>
                 ~a</body></html>" (html-board board)))
    html))
      

(defun html-board (board)
  (let ((tox (car (array-dimensions board)))
	(toy (cadr (array-dimensions board)))
	(html (make-array '(0) :element-type 'character
			  :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s html)
      (format s "<table style=\"font-family: monospace; font-size 5px;\" cellspacing=\"0\">~&")
      (do ((y 0 (1+ y)))
	  ((= toy y))
	(format s "<tr>")
	(do ((x 0 (1+ x)))
	    ((= tox x))
	  (format s "<td>~a</td>"
		  (if (aref board x y)
		      "O"
		      "&nbsp;")))
	(format s "<tr>~&"))
      (format s "</table>~&"))
    html))

(defun step-life (header param)
  (declare (ignore header))
  (let ((count (if (assoc 'count param)
		   (parse-integer (cdr (assoc 'count param)) :junk-allowed t)
		   1)))
    (do () ((zerop count))
      (setf *board* (generation-board *board*))
      (decf count))
    (html-board *board*)))
	
(defun random-board (header param)
  (declare (ignore header param))
  (setf *board* (randomize-board *board*))
  (html-board *board*))
  

(defun greeting (header params)
  (declare (ignore header))
  (let ((name (assoc 'name params)))
    (if (not name)
	"<form>What is your name?<input name='name' /></form>"
	(format nil "Nice to meet you, ~a!" (cdr name)))))

(defun stop (header params)
  (declare (ignore header params))
  (error "client stopped server"))

(defun html (c menu)
  (concatenate 'string "<!DOCTYPE html><html>"
	       (format nil "~{<a href=\"/~a\">~:*~a</a>~^ ~}"
		       (mapcar
			(lambda (x) (string-downcase (symbol-name x)))
			menu))  "<br/>" c "</html>"))

(defmacro create-handlers (not-found &rest rest)  
  `(cond ,@(mapcar (lambda (x) `((equal path ,(string-downcase (symbol-name x)))
				 (html (,x header params) (quote (,@rest)))))
		   rest)
	 (t (html (,not-found header params) (quote (,@rest))))))

(defun hello-request-handler (path header params)
  (princ (create-handlers
	  (lambda (header params)
	    (declare (ignore header params))
	    "Don't know page")
	  greeting stop step-life random-board)))
