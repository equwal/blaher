Blaher
---

Say stuff with a template that has variables inside. It's like
cowsay. Plus it's a macro, which is neat.

Examples
---

```lisp
(with-output-to-string (bag) (blah bag "Hi {{person}}!" :person "Joe")); => "Hi Joe!"

(blah t "Hi {{person}}! I am {{computer-assistant}}, here to help you, {{person}}, with all of your problems."
     :person "Fred"
     :computer-assistant "Eliza")

; => Hi Fred! I am Eliza, here to help you, Fred, with all of your problems.
```
```lisp
(blaher "Hi {{person}}! I am {{computer-assistant}}, here to help you, {{person}}, with all of your problems.")
```
macro expands to:
```lisp
(LAMBDA (#:STREAM.6790 &KEY ((:COMPUTER-ASSISTANT #:G6791))
         ((:PERSON #:G6792)))
  (PRINC "Hi " #:STREAM.6790)
  (PRINC #:G6792 #:STREAM.6790)
  (PRINC "! I am " #:STREAM.6790)
  (PRINC #:G6791 #:STREAM.6790)
  (PRINC ", here to help you, " #:STREAM.6790)
  (PRINC #:G6792 #:STREAM.6790)
  (PRINC ", with all of your problems." #:STREAM.6790))
```

[source](http://localhost/bauhh/bauhh.dyndns.org_8000/gilbert)
