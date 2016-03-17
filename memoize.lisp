;;; memoize --- thread safe function memoization

;; Copyright (C) Eric Schulte 2013

;; Placed in the public domain.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary

;; This package lets the particular lisp distribution handle the
;; creation of thread-safe hashes to hold memoized data, and lets
;; cl-store and flexi-streams do all the heavy lifting of hashing
;; arbitrary arguments.
;;
;; On these shoulders it builds a simple hashing system which is used
;; to hash function output by function arguments.  The API consists of
;; `memoize` which can be called to memoize a function and
;; `un-memoize` which can be called to un-memoize a function. All
;; saved memoized data is available through the `*memoized-data*`
;; array which may be persisted using cl-store.
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
;; Note: currently only SBCL and Clozure CL are supported, but other
;; distributions w/thread-safe hashes should be easy to add.
;;
;; TODO: Support memoization of generic functions (i.e., defmethod).

;;; Code:
(defpackage :memoize
  (:use :cl :flexi-streams :cl-store)
  (:export :defun-memoized
           :memoize
           :un-memoize
           :*memoized-data*
           :*memoized-functions*))

(in-package memoize)

(defvar *memoized-data* nil
  "Alist of memoization hashes keyed by function name.")

(defvar *memoized-functions* nil
  "Alist to store original pre-memoized functions.")

(defun thread-safe-hash-table ()
  "Return a thread safe hash table."
  #+sbcl
  (make-hash-table :synchronized t)
  #+ccl
  (make-hash-table :shared :lock-free)
  #-(or ccl sbcl)
  (error "unsupported implementation"))

(defun function-name (func)
  "Return the name of FUNC."
  #+sbcl
  (sb-impl::%fun-name func)
  #+ccl
  (ccl::function-name func)
  #-(or ccl sbcl)
  (error "unsupported implementation"))

(defun sxhash-global (el)
  "A version of sxhash which should work for arbitrary data structures.
Uses `cl-store:store' to hash objects.  Maybe slow for big arguments."
  (let ((out (make-in-memory-output-stream)))
    (store el out)
    (sxhash (octets-to-string (get-output-stream-sequence out)))))

(defun un-memoize (func-name)
  "Un-memoize the function identified by the symbol FUNC-NAME."
  (flet ((remove-func (alist)
           (remove-if (lambda (pair) (eql (car pair) func-name)) alist)))
    (setf (fdefinition func-name)
          (cdr (assoc func-name *memoized-functions*)))
    (setf *memoized-data* (remove-func *memoized-data*))
    (setf *memoized-functions* (remove-func *memoized-functions*))))

(defun memoize (func &key key (if-memoized :error))
  "Update FUNC so all future calls are memoized.
Optionally the output of the function specified by :KEY will be used
for hashing and equality tests in memoization.  Keyword argument
:IF-MEMOIZED may be one of :ERROR (the default) which raises an error
if FUNC is already memoized, :IGNORE which simply returns if FUNC is
already memoized and :REPLACE which will replace the memoized version
of FUNC deleting any existing memoized data."
  (let ((name (function-name func))
        (ht (thread-safe-hash-table)))
    (when (assoc name *memoized-data*)
      (case if-memoized
        (:error   (error "Function ~S is already memoized." name))
        (:replace (un-memoize name))
        (:ignore  (return-from memoize))
        (t (error "Unsupported value of :if-memoized. ~S" if-memoized))))
    (push (cons (function-name func) ht)   *memoized-data*)
    (push (cons (function-name func) func) *memoized-functions*)
    (setf (fdefinition name)
          (if key
              (lambda (&rest args)
                (let ((hash (sxhash-global (funcall key args))))
                  (or (gethash hash ht)
                      (setf (gethash hash ht) (apply func args)))))
              (lambda (&rest args)
                (let ((hash (sxhash-global args)))
                  (or (gethash hash ht)
                      (setf (gethash hash ht) (apply func args)))))))))

(defmacro defun-memoized (fn args &body body)
  "Define a memoized function FN with ARGS and BODY"
  `(progn (memoize (symbol-function (defun ,fn ,args ,@body))
                   :if-memoized :ignore)
          ',fn))
