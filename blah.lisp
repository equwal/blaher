(defun blah (stream template &rest args)
  (multiple-value-bind (format keys) (parse-blah template)
    (apply #'format stream format (loop for key in keys
                                        collect (getf args key)))))

(define-compiler-macro blah (&whole whole stream template &rest args)
  (cond ((stringp template)             ;constantp?
         `(funcall (blaher ,template) ,stream ,@args))
        (t
         whole)))

(defmacro blaher (template)
  (multiple-value-bind (format keys) (parse-blah template)
    (let ((stream (gensym "STREAM."))
          (args (gensym "ARGS."))
          (params (mapcar #'(lambda (x) (list x (gensym)))
                          (remove-duplicates keys))))
      `(lambda (,stream &key ,@(mapcar #'list params))
         (format ,stream ,format ,@(mapcar #'(lambda (key)
                                               (cadr (assoc key params)))
                                           keys))))))

(defun parse-blah (string &optional (start 0) (end (length string)))
  (labels ((format-escape (string)
             (with-output-to-string (bag)
               (loop for c across string do
                     (when (eql c #\~) (write-char c bag))
                     (write-char c bag)))))
    (let* ((p1 (search "{{" string :start2 start :end2 end))
           (p2 (and p1 (search "}}" string :start2 p1 :end2 end))))
      (cond ((and p1 p2)
             (multiple-value-bind (format args)
                 (parse-blah string (+ p2 2) end)
               (values (format nil "~A~~A~A"
                               (format-escape (subseq string start p1))
                               format)
                       (cons (intern (string-upcase
                                      (subseq string (+ p1 2) p2))
                                     :keyword)
                             args))))
            (t
             (values (format-escape (subseq string start end)) nil))))))

;; (blah nil "Hi {{person}}!" :person "Joe") => "Hi Joe!"

;; (blah nil "Hi {{person}}! I am {{computer-assistant}}, here to help you, {{person}}, with all of your problems."
;;      :person "Fred"
;;      :computer-assistant "Eliza")

;; => "Hi Fred! I am Eliza, here to help you, Fred, with all of your problems."

;; (blaher "Hi {{person}}! I am {{computer-assistant}}, here to help you, {{person}}, with all of your problems.")

;; macro expands to:

;; (LAMBDA (#:STREAM.5900 &KEY ((:COMPUTER-ASSISTANT #:G5902))
;;          ((:PERSON #:G5903)))
;;   (FORMAT #:STREAM.5900
;;           "Hi ~A! I am ~A, here to help you, ~A, with all of your problems."
;;           #:G5903
;;           #:G5902
;;           #:G5903))

