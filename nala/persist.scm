;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nala persist)
  #:use-module (oop goops)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:export (obj->sxml sxml->obj xml->obj nala:serializer nala:deserializer))

;; TODO: add binary serialization, now we just have XML one.

(define (serialize-hash-table obj)
  (hash-map->list list obj))
(define (unserialize-hash-table sxml)
  (let ((ht (make-hash-table)))
    (for-each (lambda (x)
                (hash-set! ht (car x) (cadr x)))
              sxml)
    ht))

(define (serialize-list obj)
  (map obj->sxml obj))
(define (unserialize-list sxml)
  (map sxml->obj sxml))

(define (serialize-pair obj)
  (list (obj->sxml (car obj)) (obj->sxml (cdr obj))))
(define (unserialize-pair sxml)
  (cons (sxml->obj (car sxml)) (sxml->obj (cadr sxml))))

(define (serialize-record-type obj)
  (let* ((des (record-type-descriptor obj))
         (name (record-type-name des)))
    (list name (map (lambda (x) 
                      (list x ((record-accessor des x) obj)))
                    (record-type-fields des)))))

(define (serialize-class obj) #t)
(define (unserialize-class sxml) #t)

(define (serialize-procedure obj) #t)
(define (unserialize-procedure sxml) #t)

(define (serialize-vector obj) 
  (let ((ll (vector->list obj)))
    (serialize-list ll)))
(define (unserialize-vector sxml)
  (let ((ll (unserialize-list sxml)))
    (list->vector ll)))

(define (serialize-charset obj) 
  (char-set->list obj))
(define (unserialize-charset sxml)
  (list->char-set sxml))

(define (serialize-bitvector obj) 
  (bitvector->list obj))
(define (unserialize-bitvector sxml)
  (list->bitvector sxml))

(define (serialize-array obj)
  (array->list obj))
(define (unserialize-array sxml)
  (let* ((ndim (car sxml))
         (ll (cdr sxml)))
    (list->array ndim ll)))

(define generic-serializer identity)
(define generic-deserializer identity)

(define *type-list*
  `((,number? number ,generic-serializer ,generic-deserializer)
    (,char? char ,generic-serializer ,generic-deserializer)
    (,string? string ,generic-serializer ,generic-deserializer)
    (,list? list ,serialize-list ,unserialize-list)
    (,pair? pair ,serialize-pair ,unserialize-pair)
    ;;(,class? class ,serialize-class ,unserialize-class)
    (,procedure? procedure ,serialize-procedure ,unserialize-procedure)
    (,vector? vector ,serialize-vector ,unserialize-vector)
    (,keyword? keyword ,generic-serializer ,generic-deserializer)
    (,char-set? charset ,serialize-charset ,unserialize-charset)
    (,symbol? symbol ,generic-serializer ,generic-deserializer)
    (,record-type? recordtype ,serialize-record-type #f)
    (,bitvector? bitvector ,serialize-bitvector ,unserialize-bitvector)
    (,array? array ,serialize-array ,unserialize-array)
    (,boolean? boolean ,generic-serializer ,generic-deserializer)
    (,hash-table? hashtable ,serialize-hash-table ,unserialize-hash-table)))

(define (get-type-ctx obj)
  (any (lambda (x) (and ((car x) obj) (cdr x))) *type-list*))

(define* (obj->sxml obj #:optional (serializer #f))
  (let* ((ctx (get-type-ctx obj))
         (type (car ctx))
         (ser (or serializer (cadr ctx))))
    (if ser
        `(,type ,(ser obj))
        (error "This type need a serializer!" type))))

(define (get-deserializer type)
  (any (lambda (x) (and (eqv? type (cadr x)) (cadddr x))) *type-list*))

(define* (sxml->obj sxml #:optional (deserializer #f))
  (let* ((type (car sxml))
         (cont (cadr sxml))
         (deser (or deserializer (get-deserializer type))))
    (if deser
        (deser cont)
        (error "This type can't be deserialiable, please give your own dersializer!" type))))

(define* (obj->xml obj #:optional (serializer #f)) 
  (sxml->xml (obj->xml obj serializer)))

(define* (xml->obj obj #:optional (deserializer #f))
  (sxml->obj (cadr (xml->sxml obj)) deserializer))

(define nala:serializer obj->xml)
(define nala:deserializer xml->obj)
