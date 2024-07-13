;; Copyright (c) 2015 Grim Schjetne
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(in-package #:json-mop)

(defclass json-serializable-class (closer-mop:standard-class) ())

(defmethod closer-mop:validate-superclass ((class json-serializable-class)
                                           (super closer-mop:standard-class)) t)

(defmethod closer-mop:validate-superclass ((class standard-class)
                                           (super json-serializable-class)) t)

(defclass json-serializable-slot (closer-mop:standard-direct-slot-definition)
  ((json-key :initarg :json-key
             :initform nil
             :reader json-key-name)
   (json-type :initarg :json-type
              :initform :any
              :reader json-type)))

(defmethod json-key-name ((slot closer-mop:standard-direct-slot-definition))
  (warn 'slot-not-serializable
        :slot-name (closer-mop:slot-definition-name slot)))

(defmethod closer-mop:direct-slot-definition-class ((class json-serializable-class)
                                                    &rest initargs)
  (declare (ignore class initargs))
  (find-class 'json-serializable-slot))

(defclass json-serializable () ())

(defmethod initialize-instance :around ((class json-serializable-class)
                                        &rest rest &key direct-superclasses)
  (apply #'call-next-method
         class
         :direct-superclasses
         (append direct-superclasses (list (find-class 'json-serializable)))
         rest))

(defmethod reinitialize-instance :around ((class json-serializable-class)
                                          &rest rest &key direct-superclasses)
  (apply #'call-next-method
         class
         :direct-superclasses
         (append direct-superclasses (list (find-class 'json-serializable)))
         rest))

(defmethod expand-slot ((rest list))
  (flet ((slot-option-p (item)
	   (member item (list :reader :write :accessor :allocation
			      :initarg :initform :type :documentation
			      :json-type :json-key))))
    (let ((first
	    (first rest))
	  (second
	    (second rest)))
      (etypecase first
	(symbol
	 (if (slot-option-p second)
	     rest
	     (let ((third (third rest)))
	       (if (or (null third) (slot-option-p third))
		   (list* first
			  :json-key second
			  (cddr rest))
		   (list* first
			  :json-key second
			  :json-type third
			  (cdddr rest))))))
	(string
	 (if (slot-option-p second)
	    (list* (intern (string-upcase (str:param-case first)))
		   :json-key first
		   (cdr rest))
	    (list* (intern (string-upcase (str:param-case first)))
		   :json-key first
		   :json-type second
		   (cddr rest))))))))

(defun slot-option-p (item)
  (member item (list :reader :write :accessor :allocation
		     :initarg :initform :type :documentation
		     :json-type :json-key)))

(deftype slot-option ()
  '(and keyword (satisfies slot-option-p)))

(defun json-mop-type-p (item)
  (member item (list :any :string :integer :number :hash-table
			  :vector :list :bool)))

(deftype json-mop-type ()
  '(and keyword (satisfies json-mop-type-p)))

(deftype non-keyword-symbol ()
  '(and symbol (not keyword)))

(defun json-mop-composite-type-p (item)
  (and (member (first item) (list :hash-table :vector :list))
       (typep (second item) '(or json-mop-type
			      non-keyword-symbol))))

(deftype json-mop-composite-type ()
  (quote
   (and cons (satisfies json-mop-composite-type-p))))

(defmacro define-json-class (name direct-superclasses direct-slots &rest options)
  (labels ((expand-slot (slot-specifier)
	     (let ((first
		     (first slot-specifier))
		   (second
		     (second slot-specifier)))
	       (etypecase first
		 (symbol
		  (etypecase second
		    (slot-option
		     slot-specifier)
		    (string
		     (let ((third
			     (third slot-specifier)))
		       (etypecase third
			 (slot-option
			  (list* first :json-key second (cddr slot-specifier)))
			 ((or json-mop-type non-keyword-symbol json-mop-composite-type)
			  (list* first :json-key second :json-type third
				 (cdddr slot-specifier))))))))
		 (string
		  (let ((slot-name
			  (intern (string-upcase (str:param-case first)))))
		    (if (= (length slot-specifier) 1)
			(list slot-name :json-key first)
			(etypecase second
			  (slot-option
			   (list* slot-name :json-key first
				  (cdr slot-specifier)))
			  ((or non-keyword-symbol json-mop-type json-mop-composite-type)
			   (list* slot-name :json-key first :json-type second
				  (cddr slot-specifier)))))))))))
    (list* 'defclass name direct-superclasses
	   (mapcar #'expand-slot direct-slots)
	   (list :metaclass 'json-mop:json-serializable-class)
	   options)))
