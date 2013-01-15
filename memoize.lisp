;;; memoize --- thread safe function memoization

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; This package lets the particular lisp distribution handle the
;; creation of thread-safe hashes to hold memoized data, and lets
;; cl-store and flexi-streams do all the heavy lifting of hashing
;; arbitrary arguments.
;;
;; On these shoulders it builds a simple hashing system exposed by
;; `memoize` which can be called to memoize a function, `un-memoize`
;; which can be called to un-memoize a function, and exposing all
;; saved memoized data in the `*memoized-data*` array which may be
;; persisted using cl-store.
;;
;;     * (defun foo (it) (format t "~S~%" it) (random 2400))
;;     FOO
;;     * (memoize #'foo)
;;     #<CLOSURE (LAMBDA (&REST ARGS) :IN MEMOIZE) {1002BC373B}>
;;     * (foo 2)
;;     2
;;     1324
;;     * (foo 2)
;;     1324
;;     * *memoized-data*
;;     ((FOO . #<HASH-TABLE :TEST EQL :COUNT 1 {1002BC3653}>))
;;     * *memoized-functions*
;;     ((FOO . #<FUNCTION FOO {1002BAE3BB}>))
;;     * (un-memoize 'foo)
;;     NIL
;;     * *memoized-functions*
;;     NIL
;;     * *memoized-data*
;;     NIL
;;     * (foo 2)
;;     2
;;     2147
;;     * (foo 2)
;;     2
;;     1573
;;
;; Note: currently only SBCL is supported, but any other distribution
;; which supports thread-safe hashes should be easy to add.

;;; Code:
(defpackage :memoize
  (:use :cl :flexi-streams :cl-store)
  (:export :memoize :un-memoize :*memoized-data*))
(in-package :memoize)

(defvar *memoized-data* nil
  "Alist of memoization hashes keyed by function name.")

(defvar *memoized-functions* nil
  "Alist to store original pre-memoized functions.")

(defun thread-safe-hash-table ()
  "Return a thread safe hash table."
  #+sbcl
  (make-hash-table :synchronized t)
  #-(or sbcl)
  (error "unsupported implementation"))

(defun function-name (func)
  "Return the name of FUNC."
  #+sbcl
  (sb-impl::%fun-name func)
  #-(or sbcl)
  (error "unsupported implementation"))

(defun sxhash-global (el)
  "A version of sxhash which should work for arbitrary data structures.
Uses `cl-store:store' to hash objects.  Maybe slow for big arguments."
  (let ((out (make-in-memory-output-stream)))
    (store el out)
    (sxhash (octets-to-string (get-output-stream-sequence out)))))

(defun memoize (func)
  "Update FUNC so all future calls are memoized."
  (let ((name (function-name func))
        (ht (thread-safe-hash-table)))
    (assert (not (assoc name *memoized-data*))
            (func *memoized-data*)
            "Function ~S is already memoized.  To re-memoize, first un-memoize."
            name)
    (push (cons (function-name func) ht)   *memoized-data*)
    (push (cons (function-name func) func) *memoized-functions*)
    (setf (fdefinition name)
          (lambda (&rest args)
            (let ((hash (sxhash-global args)))
              (or (gethash hash ht)
                  (setf (gethash hash ht) (apply func args))))))))

(defun un-memoize (func-name)
  "Un-memoize the function identified by the symbol FUNC-NAME."
  (flet ((remove-func (alist)
           (remove-if (lambda (pair) (eql (car pair) func-name)) alist)))
    (setf (fdefinition func-name)
          (cdr (assoc func-name *memoized-functions*)))
    (setf *memoized-data* (remove-func *memoized-data*))
    (setf *memoized-functions* (remove-func *memoized-functions*))))
