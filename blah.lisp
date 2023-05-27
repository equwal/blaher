(defpackage :blah
  (:use :common-lisp)
  (:export :blah :blaher))

(in-package :blah)

(defmacro with-nil-as-string-output-stream ((stream) &body body)
  (let ((orig-stream (gensym "ORIG-STREAM.")))
    `(let ((,orig-stream ,stream)
           (,stream (or ,stream (make-string-output-stream))))
       (locally ,@body)
       (unless ,orig-stream
         (get-output-stream-string ,stream)))))

(defun blah (stream template &rest args)
  (with-nil-as-string-output-stream (stream)
    (loop for x in (parse-blah template) do
          (cond ((symbolp x)
                 (princ (getf args x) stream))
                (t
                 (princ x stream))))))

(define-compiler-macro blah (&whole whole stream template &rest args)
  (cond ((stringp template)             ;constantp?
         `(funcall (blaher ,template) ,stream ,@args))
        (t
         whole)))

(defmacro blaher (template)
  (let ((template (parse-blah template)))
    (let ((stream (gensym "STREAM."))
          (params (mapcar #'(lambda (x) (list x (gensym)))
                          (remove-duplicates
                           (remove-if-not #'symbolp template)))))
      `(lambda (,stream &key ,@(mapcar #'list params))
         (with-nil-as-string-output-stream (,stream)
           ,@(mapcar (lambda (x)
                       (cond ((stringp x)
                              `(write-string ,x ,stream))
                             (t
                              `(princ ,(cadr (assoc x params)) ,stream))))
                     template))))))

(defun parse-blah (string &optional (start 0) (end (length string)))
  (let* ((p1 (search "{{" string :start2 start :end2 end))
         (p2 (and p1 (search "}}" string :start2 p1 :end2 end))))
    (cond ((and p1 p2)
           (list* (subseq string start p1)
                  (intern (string-upcase
                           (subseq string (+ p1 2) p2))
                          :keyword)
                  (parse-blah string (+ p2 2) end)))
          (t
           (list (subseq string start end))))))

;; (with-output-to-string (bag) (blah bag "Hi {{person}}!" :person "Joe")) => "Hi Joe!"

;; (blah t "Hi {{person}}! I am {{computer-assistant}}, here to help you, {{person}}, with all of your problems."
;;      :person "Fred"
;;      :computer-assistant "Eliza")

;; => Hi Fred! I am Eliza, here to help you, Fred, with all of your problems.

;; (blaher "Hi {{person}}! I am {{computer-assistant}}, here to help you, {{person}}, with all of your problems.")

;; macro expands to:

;; (LAMBDA (#:STREAM.6174 &KEY ((:COMPUTER-ASSISTANT #:G6175))
;;          ((:PERSON #:G6176)))
;;   (WRITE-STRING "Hi " #:STREAM.6174)
;;   (PRINC #:G6176 #:STREAM.6174)
;;   (WRITE-STRING "! I am " #:STREAM.6174)
;;   (PRINC #:G6175 #:STREAM.6174)
;;   (WRITE-STRING ", here to help you, " #:STREAM.6174)
;;   (PRINC #:G6176 #:STREAM.6174)
;;   (WRITE-STRING ", with all of your problems." #:STREAM.6174))

