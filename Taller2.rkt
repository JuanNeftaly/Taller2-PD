; Juan Neftaly Castellanos Hernandez.
; 00182222
#lang racket

; EJERCICIOS
; ejercicio 1. contar elementos positivos
(define (contarPositivos lista)
  (length (filter (lambda (x) (> x 0)) lista)))

; ejercicio 2. cuadrados pares
(define (cuadradosPares lista)
  (map (lambda (x) (* x x))
       (filter even? lista)))

; ejercicio 3. el factorial
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; ejercicio 4. elevar al cubo un numero
(define (cubos lista)
  (map (lambda (x) (* x x x)) lista))

; ejercicio 5. sumar impares
(define (sumarImpares lista)
  (foldl + 0 (filter odd? lista)))

; ejercicio 6. hay negativos en la lista
(define (hayNegativo lista)
  (ormap (lambda (x) (< x 0)) lista))

; ejercicio 7. suma acumulada
(define (sumaAcumulada lista)
  (reverse
   (foldl
    (lambda (x acc)
      (cons (+ x (if (null? acc) 0 (car acc))) acc))
    '()
    lista)))

; ejercicio 8. concatenar cadenas nacionales
(define (concatenarCadenas lista)
  (foldr string-append "" lista))

; ejercicio 9. dobles de numeros mayores a "en el c*lo te la inco"
(define (doblesMayores5 lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))

; ejercicio 10. invertir lista
(define (invertir lista)
  (foldl (lambda (x acc) (cons x acc)) '() lista))

; ejercicio 11. funcion como parametro
(define (aplicarFuncion f lista)
  (map f lista))

; ejercicio 12. promedio de numeros mayores a 5
(define (promedioMayores5 lista)
  (let* ([mayores (filter (lambda (x) (> x 5)) lista)]
         [suma (foldl + 0 mayores)])
    (exact->inexact (/ suma (length mayores)))))


;Las pruebas

;; Prueba Ejercicio 1
(display "Ejercicio 1 - Contar positivos: ")
(contarPositivos '(3 -2 7 0 -5 9))

;; Prueba Ejercicio 2
(display " Ejercicio 2 - Cuadrados pares: ")
(cuadradosPares '(1 2 3 4 5 6 7 8))

;; Prueba Ejercicio 3
(display " Ejercicio 3 - Factorial: ")
(factorial 5)

;; Prueba Ejercicio 4
(display " Ejercicio 4 - Cubos: ")
(cubos '(2 3 4))

;; Prueba Ejercicio 5
(display " Ejercicio 5 - Sumar impares: ")
(sumarImpares '(1 2 3 4 5 6 7))

;; Prueba Ejercicio 6
(display " Ejercicio 6 - Hay negativos?: ")
(hayNegativo '(5 9 -3 2)) ; Debe devolver #t

;; Prueba Ejercicio 7
(display " Ejercicio 7 - Suma acumulada: ")
(sumaAcumulada '(1 2 3 4))

;; Prueba Ejercicio 8
(display " Ejercicio 8 - Concatenar cadenas: ")
(concatenarCadenas '("Hola" " " "Mundo"))

;; Prueba Ejercicio 9
(display " Ejercicio 9 - Dobles mayores a 5: ")
(doblesMayores5 '(3 6 8 2 10))

;; Prueba Ejercicio 10
(display " Ejercicio 10 - Invertir lista: ")
(invertir '(1 2 3 4))

;; Prueba Ejercicio 11
(display " Ejercicio 11 - Aplicar función: ")
(aplicarFuncion (lambda (x) (* x x)) '(1 2 3 4))

;; Prueba Ejercicio 12
(display " Ejercicio 12 - Promedio mayores a 5: ")
(promedioMayores5 '(3 8 10 4 9 2 7))
