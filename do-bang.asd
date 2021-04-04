(asdf:defsystem #:do-bang
  :description "A do extension with iterators."
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:trivial-extensible-sequences)
  :components
    ((:module lisp
      :serial t
      :components
        ((:file "packages")
         (:file "do"))))
  . #+asdf3
      (:version "0.1"
       :bug-tracker "https://github.com/yitzchak/do-bang/issues")
    #-asdf3 ())

