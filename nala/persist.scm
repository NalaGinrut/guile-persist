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
  #:export (obj->sxml obj->xml))

(define (serilize-hash-table obj)
  (hash-map->list list obj))
(define (unserilize-hash-table obj)
  #t)

(define (serilize-list obj)
  (map ->sxml obj))
(define (unserilize-list obj)
  #t)

(define (serilize-pair obj)
  (list (->sxml (car obj)) (->sxml (cdr obj))))
(define (unserilize-pair obj)
  #t)

(define (serilize-record-type obj)
  (let ((des (record-type-descriptor rt)))
    (map (lambda (x) 
           (list x ((record-accessor des x) rt)))
         (record-type-fields des))))
(define (unserilize-record-type obj)
  #t)

(define (serilize-class obj) #t)
(define (unserilize-class obj) #t)

(define (serilize-procedure obj) #t)
(define (unserilize-procedure obj) #t)

(define (serilize-vector obj) #t)
(define (unserilize-vector obj) #t)

(define (serilize-charset obj) #t)
(define (unserilize-charset obj) #t)

(define (serilize-bitvector obj) #t)
(define (unserilize-bitvector obj) #t)

(define (serilize-array obj) #t)
(define (unserilize-array obj) #t)

(define *type-list*
  `((,number? number #f #f)
    (,char? char #f #f)
    (,string? string #f #f)
    (,list? list ,serilize-list ,unserilize-list)
    (,pair? pair ,serilize-pair ,unserilize-pair)
    ;;(,class? class> ,serilize-class ,unserilize-class)
    (,procedure? procedure ,serilize-procedure ,unserilize-procedure)
    (,vector? vector ,serilize-vector ,unserilize-vector)
    (,keyword? keyword #f #f)
    (,char-set? charset ,serilize-charset ,unserilize-charset)
    (,symbol? symbol #f #f)
    (,record-type? recordtype ,serilize-record-type ,unserilize-record-type)
    (,bitvector? bitvector> ,serilize-bitvector ,unserilize-bitvector)
    (,array? array ,serilize-array ,unserilize-array)
    (,boolean? boolean #f #f)
    (,hash-table? hashtable> ,serilize-hash-table ,unserilize-hash-table)))

(define (get-type-ctx obj)
  (any (lambda (x) (and ((car x) obj) (cdr x))) *type-list*))

(define (obj->sxml obj)
  (let* ((ctx (get-type-ctx obj))
         (type (car ctx))
         (serilize (cadr ctx)))
    `(,type ,(if serilize (serilize obj) obj))))

(define (obj->xml obj) (sxml->xml (obj->xml obj)))
