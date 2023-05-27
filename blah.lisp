(defpackage :blah
  (:use :common-lisp)
  (:export #:blah #:blaher))

(in-package :blah)

(defun blah (stream template &rest args)
  (loop for x in (parse-blah template) do (princ (getf args x x) stream)))

(define-compiler-macro blah (&whole whole stream template &rest args)
  (if (stringp template)
      `(funcall (blaher ,template) ,stream ,@args)
      whole))

(defmacro blaher (template)
  (let* ((template (parse-blah template))
         (stream (gensym "STREAM."))
         (params (mapcar #'(lambda (x) (list x (gensym)))
                         (remove-duplicates
                          (remove-if-not #'symbolp template)))))
    `(lambda (,stream &key ,@(mapcar #'list params))
       ,@(mapcar (lambda (x)
                   `(princ ,(or (cadr (assoc x params)) x) ,stream))
                 template))))

(defun parse-blah (string &optional (start 0))
  (let* ((p1 (search "{{" string :start2 start))
         (p2 (and p1 (search "}}" string :start2 p1))))
    (cons (subseq string start (and p2 p1))
          (and p2 (cons (intern (string-upcase
                                 (subseq string (+ p1 2) p2))
                                :keyword)
                        (parse-blah string (+ p2 2)))))))

;; (blaher "Hi {{person}}! I am {{computer-assistant}}, here to help you, {{person}}, with all of your problems.")

;; macro expands to:

;; (LAMBDA (#:STREAM.6790 &KEY ((:COMPUTER-ASSISTANT #:G6791))
;;          ((:PERSON #:G6792)))
;;   (PRINC "Hi " #:STREAM.6790)
;;   (PRINC #:G6792 #:STREAM.6790)
;;   (PRINC "! I am " #:STREAM.6790)
;;   (PRINC #:G6791 #:STREAM.6790)
;;   (PRINC ", here to help you, " #:STREAM.6790)
;;   (PRINC #:G6792 #:STREAM.6790)
;;   (PRINC ", with all of your problems." #:STREAM.6790))
