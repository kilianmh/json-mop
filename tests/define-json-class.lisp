;; Copyright (c) 2024 Grim Schjetne
;; See COPYING for license information

(in-package #:json-mop-tests)

(def-suite define-json-class
  :in test-all
  :description "Test define-json-class macro")

(in-suite define-json-class)

(test macroexpansion
  (is-true
   (equal
    (macroexpand-1
     '(define-json-class test-class ()
       ("first")
       ("second" :documentation "This is the second slot")
       ("third")))))
  (is-true
   (equal
    '(defclass book nil
      ((testing :json-key "testing")
       (title :json-key "title"
	      :json-type :string
	      :initarg :title)
       (year-published :json-key "year_published"
		       :json-type :number
		       :initarg :year)
       (fiction :json-key "is_fiction"
		:json-type :bool
		:initarg :fiction)
       (author-name :json-key "author_name"
		    :json-type :string
		    :initform "Dummy"))
      (:metaclass json-serializable-class)
      (:documentation "Hello"))
    (macroexpand-1
     '(define-json-class book ()
       (("testing")
	(title :json-key "title" :json-type :string :initarg :title)
	("year_published" :number :initarg :year)
	(fiction "is_fiction" :bool :initarg :fiction)
	("author_name" :json-type :string :initform "Dummy"))
       (:documentation "Hello")))))
  (is-true
   (equal
    '(defclass library ()
      ((city-name :json-key "city.name"
		  :json-type city
		  :initarg :city)
       (library-type :json-key "libraryType"
		     :json-type library-type
		     :initarg :library-type))
      (:metaclass json-serializable-class)
      (:documentation "Test"))
    (macroexpand-1
     '(define-json-class library ()
       (("city.name" city :initarg :city)
	("libraryType" :json-type library-type
		       :initarg :library-type))
       (:documentation "Test"))))))

(test inheritance
  (define-json-class json-class-1 ()
    (("foo" :number :initform 1)))
  (define-json-class json-class-2 (json-class-1)
    ((title :json-key "title" :json-type :string :initarg :title)
     ("year_published" :number :initarg :year)
     (fiction "is_fiction" :bool :initarg :fiction)
     ("author_name" :json-type :string :initform "Dummy")))
  (let* ((instance (make-instance 'json-class-2))
         (parsed (yason:parse (json-string instance))))
    (is (= 1 (gethash "foo" parsed)))))
