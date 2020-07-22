;;;; package.lisp

(defpackage #:mfcc
  (:use #:cl)
  (:shadowing-import-from #:iterate #:terminate #:in)
  (:shadowing-import-from #:alexandria-2 #:flatten)
  (:shadowing-import-from #:vgplot #:range))


(in-package #:mfcc)
