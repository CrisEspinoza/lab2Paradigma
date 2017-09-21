#lang r6rs
(import (rnrs))
(define null (list))

; ---------------------- * TDA SHIPS * -------------------------
;    * Representacion

#|
- SHIP: Un barco que se colocará en el tablero que va a estar contenido
      por coordenadas x e y, ademas del nombre del barco que se quiere colocar en esa posicion.
|#

;    * CONSTRUCTORES 

#| 
- Se cran las posiciones de los barcos: Tenemos que nos genera las posiciones de los barcos a colocar y ademas se encarga de que las posiciones
  no se repitan.
- Entrada -> * ship : Lista que contiene las coordenadas (x e y) y el nombre del barco el cual se encuentra posiciona en esa posicion
             * N y M : Dimensiones del tablero que se genera
             * posicionesB : Lista la cual retornara la lista final que contendra las posiciones de los barcos y el nombre de el, pero con
                     la caracteristica que no se sobrepongan
             * seed : Semilla con la cual se van ir pidiendo nuevas coordenadas en el caso que alguna se repita.
- Salido : Retornara una lista que contendra las coordenadas (x e y) y el nombre del barco que se encuentre en esa posicion con la
      caracteristica que todas las posiciones de los barcos seran diferentes
- Recursion que se utiliza: En este caso ocuparemos recursion de cola para poder generar la plantilla de los barcos, con que
                    durante el transcurso de la partida se van a ir utilizando
|#
(define makeBarcos
  (lambda (ship N M posicionB seed)
    (let ((x (car(getListaRandom 1 (+ seed 1) (/ N 2)))))
      (let ((y (car(getListaRandom 1 (+ 1 seed) M))))
        (if(and(numero N)(and(numero M) (and (numero seed) (lista posicionB) )))
           (if(null? ship) posicionB
              (if (listaDeBarco ship) 
                  (if(= 1 (length ship)) (makeBarcos (cdr ship) N M (append posicionB (list (car ship))) (+ 1 seed))
                     (if(verificar (car(car ship)) (car(cdr(car ship))) (cdr ship))
                        (makeBarcos (cdr ship) N M (append posicionB (list(car ship)) ) (+ 1 seed) )
                        (makeBarcos (append (cdr ship) (list(list x y (car(cdr(cdr(car ship)))) ))) N M posicionB (+ 1 seed) )
                        )
                     )
                  #f
                  )
              )
           #f
           )
        )
      )
    )
  )


;    * PERTENENCIA

#|
Verifica que x sea de tipo char
Entrada-> x : Objeto que se va a verificar si es un char o no
Salida -> booleano que representaara si es un char o no. 
|#
(define caracter
  (lambda(x)
    (if(char? x ) #t
       #f)
    )
  )

#|
Verifica que x sea de tipo numerico
Entrada-> x : Objeto que se va a verificar si es un numero o no
Salida -> booleano que representaara si es un numero o no. 
|#
(define numero
  (lambda(x)
    (if(number? x ) #t
       #f)
    )
  )

#|
Verifica que x sea de tipo pares
Entrada-> x : Objeto que se va a verificar si es un pares o no
Salida -> booleano que representaara si es un pares o no. 
|#
(define par
  (lambda(x)
    (if(pair? x )
       (if (and (numero (car x))(numero (cdr x))) #t
       #f)
       )
    )
  )

#|
Verifica que x sea una lista de tipo barcos (x y barco).
Entrada-> lista : lista que se va a verificar que sea una lista de tipo barco.
Salida -> booleano que representara si es una lista de tipo barco o no.
|#
(define listaDeBarco
  (lambda(lista)
    (if(list? lista)
       (if (null? lista) #t
           (if (and (numero (puntoX lista)) (and ( numero (puntoY lista)) (caracter (barcoDeLista lista)) ))
               #t
               #f
               )
           )
       )
    )
  )

#|
Verifica que x sea una lista de pares
Entrada-> x : Objeto que se va a verificar si es una lista de pares o no
Salida -> booleano que representaara si es una lista pares o no. 
|#
(define listaDePares
  (lambda(lista)
    (if(list? lista)
       (if (null? lista) #t
           (if (list? (car lista))
               (listaDePares (cdr lista))
               #f)
           )
       )
    )
  )

#|
Verifica que x sea una lista
Entrada-> x : Objeto que se va a verificar si es una lista o no
Salida -> booleano que representaara si es una lista o no. 
|#
(define lista
  (lambda(x)
    (if(list? x)
       #t
       #f
       )
    )
  )

;    * SELECTORES

#|
- Obtiene la coordenada x de un par
- Entrada -> lista : Lista de donde se sacara la coordenada x
- Salida -> Numero de la lista que representa el elemento x
|#
(define puntoX
  (lambda (lista)
    (if (listaDePares lista)
        (car(car lista))
        #f
        )
    )
  )

#|
- Obtiene la coordenada y de un par
- Entrada -> lista : Lista de donde se sacara la coordenada y
- Salida -> Numero de la lista que representa el elemento y
|#
(define puntoY
  (lambda (lista)
    (if (listaDePares lista)
        (car(cdr(car lista)))
        #f
        )
    )
  )

#|
- Obtiene el barco de la primera posicion de la lista
- Entrada -> lista : Lista de donde se sacara el primer barco
- Salida -> Barco que correspende a la primera posicion de la lista 
|#
(define barcoDeLista
  (lambda (lista)
    (if (listaDePares lista)
        (if (null? lista) #f
            (car(cdr(cdr(car lista))))
            )
        )
    )
  )

;    * MODIFICADORES

#|
- Funcion que va a modificar la lista de ship cuando no se cumplan las condiciones optimas
- Entrada -> * ship : Lista de barcos que se estan verificando que no estes sobrepuestos
             * x e y : Coordenadas que se generan nuevamente al momento que alguna de las posiciones esten repetidas, esto con el fin de
                 evitar que los barcos se sobrepongan
- Salida-> Entregara la lista modificada, los cambios que va a realizar seran que entregara la cola de la lista anterior, sin las posiciones
     que se estaban repitiendo, ademas de mantener el mismo nombre del barco pero con unas coordenadas distintas generadas por el random.
|#
(define modificarLista
  (lambda (ship x y )
    (if (and (numero x) (and( numero y ) (listaDeBarco ship)))
        (append (cdr ship) (list(list x y (barcoDeLista ship) )))
        #f
        )
    )
  )

#|
- Funcion que va a modificar la lista que inicialmente estaba vacia
- Entrada-> * lista : Lista de la cual se van a sacar las posiciones y el nombre del barco que se va agregar a la nueva lista de barcos
            * posicionesB : Esta es la lista la cual se van ir agregando los barcos con sus respectivos nombres y coordenadas que no seran
                   repetidas, para asi evitar que se sobrepongan.
- Salida-> Entregara la lista que se le agrego los barcos con sus respectivas coordenadas.
|# 
(define modificarListaBarco
  (lambda (lista posicionB)
    (if(listaDeBarco lista)
       (append posicionB (list(list (puntoX lista)(puntoY lista) (barcoDeLista lista) )))
       #f
       )
    )
  )

#|
- Funcion que va a modificar la lista que inicialmente estaba vacia
- Entrada-> * ship : Lista de la cual se van a sacar el barco que se va agregar a la otra lista 
            * x e y : Estan son las coordenadas con las cuales se va a representar la posicion que tomara el barco
            * s : Lista donde se iran almacenando las coordenadas generadas y el nombre respectivo del barco
- Salida-> Entregara la lista que se modifico y se agrego la posicion de los barcos con su respectivo nombre
|# 
(define modificarListaShip
  (lambda (x y ship s)
    (if (and (numero x) (and (numero y) (caracter (car ship))))
        (append s (list(list x y (car ship))))
        #f
        )
    )
  )

;    * OTRAS FUNCIONES

#|
- Funcion que entregará una lista que contiene (x y barco)
- Entrada -> * ship : Nombre de los barcos que se van a crear en el juego
             * N y M : Dimensiones de la matriz que se generará
             * seed : Semilla con la cual se ira actualizando el random
             * s : Lista de listas qe contiene las coordenadas y el nombre del barco
- Salida : Lista que contendra las coordenadas x e y, ademas del nombre de barco que se encontrara en esa posición
- Recursion que se utiliza: En este caso ocupamos recursion de cola, con el fin de crear la lista de barcos previa,
                 se ocupa esta recursion, ya que para el problema se puede manejar de mejor forma.
|#
(define agregarBarcos
  (lambda (ship N M s seed)
    (if(null? ship) s
       (let ((x (car(getListaRandom 1 (+ seed 78) (- (/ N 2) 1)))))
         (let ((y (car(getListaRandom 1 (+ 14 seed) (- M 1)))))
           (agregarBarcos (cdr ship) N M (modificarListaShip x y ship s) (+ seed 1))
           )
         )
       )
    )
  )

#| Funcion que se encarga de buscar si la coordenada actual, se encuentra ubicado algun barco
- Entrada -> * x e y : Son las coordenas donde se enceuntran creando el tablero, las cuales sirven para verificar en la posicion
       correcta donde se tiene que colocar le barco
- posBarcos : Esta es una lista que contiene las dos coordenadas tanto (x , y) las cuales representan las posiciones
aleatorias que salieron para colocar el barco de la computadora
- Recursion que se utiliza: En este caso ocuparemos recursion de cola, para ir verificando si las posiciones de los barcos
                creados son iguales.
|#
(define verIgualesB
  (lambda ( x y posBarcos)
    (let ((barco (barcoDeLista posBarcos)))
      (if (null? posBarcos) #f
          (if(and(numero x) (numero y))
             (if (and (= x (puntoX posBarcos)) (= y (puntoY posBarcos))) barco
                 (verIgualesB x y (cdr posBarcos))
                 )
             #f
             )
          )
      )
    )
  )

#| Verificar posiciones de la lista generada que no sea iguales
- Entrada -> * x e y : Coordenadas que se van a verificar si son iguales a las de la lista
             * lista : Lista con la cual se van hacer las comparaciones de las coordenadas x e y
- Salida -> Retornara booleano que representara si es igual a alguna coordenada de la lista o no
- Recursion que se utiliza: En este caso ocuparemos recursion de cola para ir verificando las coordenadas en donde
           se van a colocar los barcos para ver si estan disponibles y no sobrepondra a otra embarcación
|#
(define verificar
  (lambda(x y lista)
    (if(null? lista) #t
       (if(and(numero x)(numero y)) 
          (if(and (= x (puntoX lista)) (= y (puntoY lista)) ) #f 
             (cond
               ((= 1 (length lista)) #t)
               (else (verificar x y (cdr lista))))
             )
          #f
          )
       )
    )
  )

;Estas constantes fueron sacadas de https://en.wikipedia.org/wiki/Linear_congruential_generator
(define a 1103515245)
(define c 12345)
(define m 2147483648)

;Esta función random tuma un xn y obtiene el xn+1 de la secuencia de números aleatorios.
(define myRandom
  (lambda
    (xn)
    (mod (+ (* a xn) c) m)
  )
)
;Cada vez que pedimos un random, debemos pasar como argumento el random anterior.


;Acá un ejemplo que permite generar una lista de números aleatorios.
;Parámetros:
;* "cuantos" indica el largo de la lista a generar.
;* "xActual" valor actual del random, se pasa en cada nivel de recursión de forma actualizada
;* "maximo" Los números generados van desde 0 hasta maximo-1
(define getListaRandom
  (lambda (cuantos xActual maximo)
    (if (= 0 cuantos)
        '()
        (let ((xNvo (myRandom xActual)))
          (cons (mod xNvo maximo)
              (getListaRandom (- cuantos 1) xNvo maximo)
          )
        )
    )
  )
)

#| Funcion que se encarga de verificar si la coordenada actual es igual a la que se esta buscando para colocar el barco 
- x e y : Son las coordenas donde se enceuntran creando el tablero, las cuales sirven para verificar en la posicion
       correcta donde se tiene que colocar le barco
- posBarcos : Esta es una lista que contiene las dos coordenadas tanto (x , y) las cuales representan las posiciones
aleatorias que deben ser iguales a las actuales para poder proceder a colocar el barco 
|#
(define verIguales
  (lambda ( x y posBarcos)
    (if (null? posBarcos) #f
        (if(and(numero x)(numero y))
           (if (and (= x (car(car posBarcos))) (= y (car(cdr(car posBarcos))))) #t
               (verIguales x y (cdr posBarcos))
               )
           #f
           )
        )
    )
  )

; ---------------------------- * TDA TABLERO * -----------------------------------


;    * Representacion

#|
- Tablero-> ((x y (nombre)) () )  
- x : Representa la coordenada x del tablero
- y : Representa la coordenada y del tablero
- nombre: El nombre del barco que estara ocupando la casilla, tener en cuenta que tambien puede tener:
        * #\X -> Que va a representar que esa coordenada ya fue atacada
        * #\- -> Que va a representar que esa corrdenada esta vacia  (tiene agua)
        * () -> Entregara la ultima jugada que se realizo en el tablero
|#

;    * CONSTRUCTORES 

#|
 * Tablero por recursion de Lineal

- N : Numero de filas
- M : Numero de columnas
- ships : Lista de barcos
- Declaramos un let, para poder almacenar las dos variables y e x, luego inicializarlas en valor 0, ademas de colocar
    una lista vacia para poder ir almacenando el tablero
- En esta funcion que crea el tablero ocupamos la recursion lineal para crearlo, como nos podemos dar cuenta que es una recursion 
   lineal bueno tenemos que va dejando estados pendiende , ya que claramente se puede ver cuando se crear los pares (cons).
   Principalmente se crea de esta manera para cumplir con los requisitos del enunciado.
|#
(define createBoardRL
 (lambda (N M ships)
   (if(and(numero N)(and(numero M)(listaDeBarco ships)))
      (let recur ((x 0)(y 0) (posBarco ships) )
        (if(and(> N 0)(> M 0))
           (if (and(= x N) (= y M)) null
               (if(>= x N) null
                  (cond
                    ((< y (- M 1)) (if(null? posBarco) (recur x (+ y 1) posBarco)
                                      (if(verIgualesBa x y posBarco)
                                         (cons (list x y (verIgualesBar x y posBarco))(recur x (+ y 1) posBarco))
                                         (cons (list x y #\-)(recur x (+ y 1) posBarco))
                                         )
                                      )
                                   )
                    (else (if(null? posBarco) (recur (+ x 1) 0 posBarco)
                             (if(verIgualesBa x y posBarco)
                                (cons (list x y (verIgualesBar x y posBarco))(recur (+ x 1) 0 posBarco))
                                (cons (list x y #\-)(recur (+ x 1) 0 posBarco))
                                )
                             )
                          )
                    )
                  )
               )
           #f
           )
        )
      )
   )
  )
  

#| * Tablero por recursion de cola

- N : Numero de filas
- M : Numero de columnas
- ships : Lista de barcos
- Declaramos un let, para poder almacenar las dos variables y e x, luego inicializarlas en valor 0
   ademas de colocar una lista vacia para poder ir almacenando el tablero
- En esta funcion que crea el tablero ocupamos recursion de cola, como nos podemos dar cuenta que se crea de esa manera:
   * No va dejando estados pendientes y va llamando en cada paso a la misma funcion, con el fin de cuando llegue al caso
      base, solamente entregara una respuesta que sera la solicitada.
  Principalmente se hizo esto para poder cumplir con los requisios minimos que fueron planteados en el enunciado

|#
(define createBoardRC
 (lambda (N M ships)
   (let recur((x 0)(y 0) (board '()) (posBarco ships) )
     (if (and (> N 0) (> M 0))
         (if (and(= x N) (= y M)) board
             (if(>= x N) board
                (cond
                  ((< y (- M 1)) (if(null? posBarco) (recur x (+ y 1) (colocarAgua x y board) posBarco)
                                    (if(verIgualesBa x y posBarco)
                                       (recur x (+ y 1) (colocarBarco x y board posBarco) posBarco)
                                       (recur x (+ y 1) (colocarAgua x y board) posBarco)
                                       )
                                    )
                                 )
                  (else (if(null? posBarco) (recur (+ x 1) 0 (colocarAgua x y board) posBarco)
                           (if(verIgualesBa x y posBarco)
                              (recur (+ x 1) 0 (colocarBarco x y board posBarco) posBarco)
                              (recur (+ x 1) 0 (colocarAgua x y board) posBarco)
                              )
                           )
                        )
                  )
                )
             )
         #f
         )
     )
   )
  )

;    * PERTENENCIA

#|
Buscar barco que ralizara el ataque si se encuentra en el tablero
- Entrada -> * ship : Barco que realizara el ataque se verificasi se encuentra en el tableero
             * tablero : Tablero donde se buscara que el barco exista
- Salida-> Retorna valor baooleano que indicara si esta o no el barco en el tablero
- En esta funcion ocupamos la recursion de cola, con el fin de buscar el barco que realizara el ataque y se encuentra
    en el tablero (para evitar que no este o ya fuera hundido)
|#
(define busAtak
  (lambda (ship tablero)
    (if(and(caracterB ship)(listaDeBarcoB tablero))
       (let recur ((barco ship) (board tablero))
         (if(null? board) #f
            (if (char=? barco (barcoDeListaB board)) #t
                (recur barco (cdr board))
                )
            )
         )
       #f
       )
    )
  )

#|
Funcion que se encaraga de verificar si el tablero es valido para la partida.
- Entrada -> * Board : Tablero que se verificara si es un tablero valido o no
- Salida-> Retorna valor baooleano que indicara si el tablero es valido o no
|#
(define checkBoard
  (lambda(board)
    (let (( x (car(car(reverse board)))))
        (if (and (even? (+ 1 x)) (listaDeBarco board)) #t #f
            )
        )
    )
  )

#|
Verifica que x sea de tipo char
Entrada-> x : Objeto que se va a verificar si es un char o no
Salida -> booleano que representaara si es un char o no. 
|#
(define caracterB
  (lambda(x)
    (if(char? x ) #t
       #f)
    )
  )

#|
Verifica que x sea de tipo numerico
Entrada-> x : Objeto que se va a verificar si es un numero o no
Salida -> booleano que representaara si es un numero o no. 
|#
(define numeroB
  (lambda(x)
    (if(number? x ) #t
       #f)
    )
  )

#|
Verifica que x sea una lista de pares
Entrada-> x : Objeto que se va a verificar si es una lista de pares o no
Salida -> booleano que representaara si es una lista pares o no. 
|#
(define parB
  (lambda(lista)
    (if(pair? lista)
       #t
       #f
       )
    )
  )

#|
Verifica que x sea una lista de tipo barcos (x y barco).
Entrada-> lista : lista que se va a verificar que sea una lista de tipo barco.
Salida -> booleano que representara si es una lista de tipo barco o no.
|#
(define listaDeBarcoB
  (lambda(lista)
    (if(list? lista)
       (if (null? lista) #t
           (if (and (numero (puntoXB lista)) (and ( numero (puntoYB lista)) (caracter (barcoDeLista lista)) ))
               #t
               #f
               )
           )
       )
    )
  )

;    * SELECTORES

#|
- Obtiene la coordenada x de un par
- Entrada -> lista : Lista de donde se sacara la coordenada x
- Salida -> Numero de la lista que representa el elemento x
|#
(define puntoXB
  (lambda (lista)
    (if (listaDePares lista)
        (car(car lista))
        #f
        )
    )
  )

#|
- Obtiene la coordenada y de un par
- Entrada -> lista : Lista de donde se sacara la coordenada y
- Salida -> Numero de la lista que representa el elemento y
|#
(define puntoYB
  (lambda (lista)
    (if (listaDePares lista)
        (car(cdr(car lista)))
        #f
        )
    )
  )

#|
- Obtiene el barco de la primera posicion de la lista
- Entrada -> lista : Lista de donde se sacara el primer barco
- Salida -> Barco que correspende a la primera posicion de la lista 
|#
(define barcoDeListaB
  (lambda (lista)
    (if (listaDePares lista)
        (if (null? lista) #f
            (car(cdr(cdr(car lista))))
            )
        )
    )
  )

#|
- Obtiene el punto de una coordenada
- Entrada -> lista : Lista de donde se sacara el punto x de la coordenada entregada
- Salida -> Retorna el valor de la coordenada x de la coordenada ingresada
|#
(define punto-X
  (lambda (lista)
    (if (null? lista) #f
        (car lista)
        )
    )
  )

#|
- Obtiene el punto de una coordenada
- Entrada -> lista : Lista de donde se sacara el punto y de la coordenada entregada
- Salida -> Retorna el valor de la coordenada y de la coordenada ingresada
|#
(define punto-Y
  (lambda (lista)
    (if (null? lista) #f
        (car(cdr lista))
        )
    )
  )


;    * MODIFICADORES

#|
- Genera una lista cuando lo que se agregara al tablero sera mar
- Entrada -> * x e y : Coordenadas donde se colocara mar
             * Board: Tablero que se va ir modificando
- Salida -> Entregara un nuevo tablero con las coordenadsa y barcos modificadas 
|#
(define colocarAgua
  (lambda (x y board)
    (if (and (numero x) (and (numero y) (listaDeBarco board)))
        (append board (list(list x y #\-)))
        #f
        )
    )
  )

#|
- Genera una lista cuando lo que se agregara al tablero sera un barco
- Entrada -> * x e y : Coordenadas donde se colocara barco
             * Board: Tablero que se va ir modificando
- Salida -> Entregara un nuevo tablero con las coordenadsa y barcos modificadas 
|#
(define colocarDisparo
  (lambda (x y board)
    (if (and (numero x) (and (numero y) (listaDeBarco board)))
        (append board (list(list x y #\x)))
        #f
        )
    )
  )

#|
- Buscar posiciones que se realizo el ataque en el tablero
- Entrada -> * x e y : Posiciones que se buscan en el tablero para realizar la modificacion
             * Tablero: Tablero donde se realizaran las modificaciones
- Salida -> Entregara el nuevo tablero generado, que se va a modificar en las coordenadas ingresadas
- En esta funcion ocupamos la recursion de cola con el fin de buscar la posicion en especifico donde se realizo
  el disparo, se puede ver que ocupamos recursion de cola, ya que cada ves que se termina el proceso se vuelve a llamar
  a la misma funcion, pero cada ves entregando una coordenada menos del tablero. Entonces cuando se termina el proceso
  solamente se entrega el nuevo tablero modificado con la posicion dond se realizo el ataque.
|#
(define verPosicionBoard
  (lambda (x y tablero)
    (if(and(numeroB x)(numeroB y) (listaDeBarcoB tablero))
       (let recur ((h 0)(k 0)(boardN '())(board tablero))
         (if (null? board) boardN
             (cond
               ((< k (car(cdr(car(reverse tablero)))))
                (if(and (equal? x h) (equal? y k))
                   (recur  h (+ 1 k) (colocarDisparo x y boardN) (cdr board))
                   (recur h (+ 1 k) (append boardN (list(list h k (barcoDeListaB board) ))) (cdr board))
                   )
                )
               (else (if(and (= x (puntoXB board) ) (= y (puntoYB board) ))
                        (recur (+ 1 h) 0 (colocarDisparo x y boardN)(cdr board))
                        (recur (+ 1 h) 0 (append boardN (list(list h k (barcoDeListaB board) ))) (cdr board))
                        )
                     )
               )
             )
         )
       #f
       )
    )
  )

#|
- Se encarga de ver la simitud de las coordenadas ingresadas se encuentra un barco en dicha posicion o no
- Entrada -> * x e y : Coordenadas donde se va a realizar e ataque
             * Tablero: Donde se va recorriendo y se ve si las posiciones ingresadas correponden a donde esta ubicado
                   un barco o solamente esta por agua o ya fue disparado.
-Salida -> Devuelve un valor booleano, dando representacion si el barco estaba en dicha posicion ingresada o no.
- Se ocupa recursion de cola en este funcion, ya que cada ves que se termina de evaluar se llama nuevamente a la misma
   funcion, pero reduciendo el tablero, para que asi pueda llegar al caso base o cumplir alguna de las condiciones.
|#
(define similitud
  (lambda (x y tablero)
    (if (and(numero x)(and (numero y) (listaDeBarco tablero)))
        (let recur ( (x x) (y y) (board tablero))
          (if ( null? board ) #f
              (if (and(equal? x (puntoXB board)) (equal? y (puntoYB board)))
                  (if (or (eq? #\x (barcoDeLista board)) (eq? #\- (barcoDeLista board)))
                      #f
                      #t)
                  (recur x y (cdr board))
                  )
              )
          )
        )
    )
  )

#|
- Bueno esta funcion trabaja en conjunto con la descrita anteriormente, es la escargada de mandar la respuesta que se
estaba pidiendo, esto se moestrara al momento de imprimir el tablero final.
-Entrada -> * board: Tablero que se esta ocupando y en donde se veran si cumple las condiciones
            * x e y : Coordenadas de donde se quierer verificar si el disparo fue exitoso o fallido
-Salida -> Entraga un valor 2 si el disparo fue ocasionado y destruyo a la embaracion y 0 si el disparo fue al agua
|#
(define verDisparo
  (lambda (board x y)
    (if (and (numero x) (and (numero y) (and (listaDeBarco board))))
        (if (null? board) 0
            (if (similitud x y board)
                2
                0
                )
            )
        )
    )
  )

#|
- Esta funcion se encarga de verificar si el disparo de la computadora fue dirigido a un barco o no
- Entrada-> * board: Tablero donde se buscara si las coordenadas cayeron en una embarcacion o no.
            * x e y: Coordenadas que se van a evaluar si fueron exitosas o no
- Salida-> Entragara un valor booleano si el disparo fue exitoso o no
|#
(define verDisparoUs
  (lambda (board x y)
    (if (and (numero x) (and (numero y) (and (listaDeBarco board))))
        (if (and (= x (puntoXB board)) (= y (puntoYB board)))
             (if (or (eq? #\x (barcoDeLista board)) (eq? #\- (barcoDeLista board)))
                      #f
                      #t)
            )
        )
    )
  )
  

#|
- Funcion play de la computadora, que se encarga de atacar en la parte enemiga del tablero
- Entrada -> * seed: Semilla que se le pasara como parametro al random
             * Board: Tablero que se va ir modificando
             * s e a :Son las coordenadas del ataque que ingreso el usuario, que se ocuparan para poder mostrar
                 el resultado que obtuvo el usuario al realizar sua ataque
             * ship: Barco que el usuario ingreso para realizar el ataque
- Salida -> Entregara un nuevo tablero con el ataque que se realizo, ademas de poder mostrar las coordenadas de cada ataque
       que se realizo y el reultado de cada uno de ellos is fue exitoso o fallido.
-En esta funcion se ocupo recursion de cola, ya que se va entregando cada ves un elemnto menos del board, para que
  asi pueda llegar al caso base y poder entregar el resultado final.
|#
(define playCp
  (lambda (tablero seed a s ship)
    (if(and (listaDeBarcoB tablero)(numeroB seed))
       (let recur ((x (posXCp seed tablero)) (y (posYCp seed tablero)) (h 0) (k 0) (boardNu '()) (board tablero) (ataque 0) (barcoEnemigo (barcoAtackOpe tablero seed)))
         (if (null? board) (list boardNu (list (list "computador" "ataque en coordenadas ""x:"a "e y:"s  "Realizado por el barco" ship ".Resultado del ataque" (verDisparo boardNu a s)) (list "usuario" "ataque en coordenadas ""x:"a "e y:"s  "Realizado por el barco" barcoEnemigo ".Resultado del ataque" ataque)))
             (cond
               ((< k (car(cdr(car(reverse tablero)))))
                (if(and (equal? x h) (equal? y k))
                   (if(verDisparoUs board x y)
                      (recur  x y h (+ 1 k) (colocarDisparo x y boardNu) (cdr board) 2 barcoEnemigo)
                      (recur  x y h (+ 1 k) (colocarDisparo x y boardNu) (cdr board) ataque barcoEnemigo)
                      )
                   (recur x y h (+ 1 k) (append boardNu (list(list h k (barcoDeListaB board) ))) (cdr board) ataque barcoEnemigo)
                   )
                )
               (else (if(and (equal? x h) (equal? y k ))
                        (if(verDisparoUs board x y)
                           (recur  x y (+ h 1) 0 (colocarDisparo x y boardNu) (cdr board) 2 barcoEnemigo)
                           (recur  x y (+ h 1) 0 (colocarDisparo x y boardNu) (cdr board) ataque barcoEnemigo)
                           )
                        (recur x y (+ 1 h) 0 (append boardNu (list(list h k (barcoDeListaB board) ))) (cdr board) ataque barcoEnemigo)
                        )
                     )
               )
             )
         )
       #f
       )
    )
  )

#|
- Funcion ue le coloco un barco en una lista entregada
- Entrada -> * x e y: Coordenadas donde se va agregar el barco ingresado
             * Board: Tablero que se va ir modificando
             * PosBarco: Lista donde se entregan los barcos que se tiene en la partida
- Salida -> Entregara un nuevo tablero con el barco colocado en la posicion correspondiente
|#
(define colocarBarco
  (lambda (x y board posBarco)
    (if (and (numero x) (and (numero y) (listaDeBarco board)))
        (append board (list(list x y (verIgualesBar x y posBarco))))
        #f
        )
    )
  )

#|
- Funcion play
- Entrada -> * ship: Barco del usuario que va a realizar el ataque
             * Board: Tablero que se va ir modificando
             * Positions: Coordenadas de la parte enemiga del tablero, donde se realizara el ataque
             * seed: Semilla que se va ir entregando a las diferentes funciones para poder ir generando los random
- Salida -> Entregara un tablero modificado con los ataques que realizo el usuario y la respuesta que tendra la computadora
|#(define play
  (lambda (board ship positions seed)
    (if (and(numeroB seed)(and(parB positions)(and(caracterB ship)(listaDeBarcoB board))))
        (if (<= (+ 1 (car positions)) (/ (+ (car(car  (reverse board ))) 1) 2) )
            (playUs (car positions) (car(cdr positions)) ship board seed)
            #f
            )
        #f
        )
    )
  )

#|
- Funcion que se encarga de verificar as entradas y llamar las funciones que realizaran las modificaciones
- Entrada -> * x e y: Coordenadas donde se va agregar el barco ingresado
             * Board: Tablero que se va ir modificando
             * ship: Barco del usuario que va a realizar el ataque
             * seed: Semilla que se va entregar a las funciones random para que puedan ir actualizandose
- Salida -> Entregara un nuevo tablero con las modificaciones realizadas
|#
(define playUs
  (lambda (x y ship board seed)
    (if(and (numeroB x)(and(numeroB y) (and(caracterB ship)(and(listaDeBarcoB ship)(numeroB seed)))))
       (if (busAtak ship board)
           (playCp (verPosicionBoard x y board) (+ 679 seed) x y ship )
           0)
       #f
       )
    )
  )

;Tenemos que tener que la primera fila y columna del tablero es el 0
#|
- Funcion que se encarga de coloar un barco en cierta posiciones donde se ingrese
- Entrada -> * position: Coordenadas de donde se va a colocar el barco
             * Board: Tablero que se va ir modificando
             * ship: Nombre del barco que se va a colocar en el tablero
- Salida -> Entregara un nuevo tablero modificado con el barco colocado en las coordenadas correspondientes
|#
(define putShip
  (lambda (board position ship)
    (if (and (caracterB ship) (and(listaDeBarcoB board) (parB position)))
        (if(and (>= (punto-X position) (/ (+ (car(car(reverse board))) 1) 2)) (<= (punto-Y position) (car(cdr(car(reverse board))))) )
           (if (<= (car position) (+ 1 (car(car(reverse board)))))
               (recorrerBoard board (punto-X position) (punto-Y position) ship)
               #f
               )
           #f)
        #f
        )
    )
  )

#|
- Funcion que se encarga de colocar el barco ingresado en la posicion que ingreso el usuario
-Entrada -> * tablero : Tablero donde se va a modificar la coordenada ingresada por le barco que ingreso el usuario
            * x e y: Coordenadas donde se va a colocar el barco
            * ship: Nombre del barco que se va acolocar en las coordenadas ingresadas
-Salida -> Entregara un nuevo tablero modificado, en el cual se encontrara el barco ingresado por el usuario en las
        coordenadas que ingreso el usuario.
- En esta funcion se ocupo recursion de cola, para poder ir generando el nuevo tablero que se entregara como salida.
|#
(define recorrerBoard
  (lambda (tablero x y ship)
    (if (and(listaDeBarcoB tablero)(and(numero x)(and(numero y) (caracterB ship))))
        (let recur((boardNuevo '()) (h 0) (k 0) (board tablero) (x x) (y y))
          (if(null? board) boardNuevo
             (cond
               ((< k (car(cdr(car(reverse board)))))
                (if(and(equal? x (puntoXB board)) (equal? y(puntoYB board)) )
                   (recur (append boardNuevo (list(list h k ship))) h (+ 1 k) (cdr board) x y)
                   (recur (append boardNuevo (list(list h k (barcoDeListaB board)))) h (+ 1 k) (cdr board) x y)
                   )
                )
               (else (if(and (equal? x (puntoXB board)) (equal? y (puntoYB board)) )
                        (recur (append boardNuevo (list(list h k ship))) (+ 1 h) 0 (cdr board) x y)
                        (recur (append boardNuevo (list(list h k (barcoDeListaB board)))) (+ 1 h) 0 (cdr board) x y )
                        )
                     )
               )
             )
          )
        #f
        )
    )
  )

;    * OTRAS FUNCIONES

#|
- Generar posicion aletoria de la coordenada y
- Entrada -> * Seed : Semilla que se va ir pasando al random
             * Board: Tablero de donde se sacaran las dimensiones para generar la posicion random
- Salida -> Entregara la coordenada x que se genera random para la computadora
|#
(define posXCp
  (lambda (seed board)
    (if(null? board) #f
       (+ (car(getListaRandom 1 (+ seed 765432) (/ (+ (car(car(reverse board))) 1) 2) )) (/ (+ (car(car(reverse board))) 1) 2))
       )
    )
  )

#|
- Generar posicion aletoria de la coordenada y
- Entrada -> * Seed : Semilla que se va ir pasando al random
             * Board: Tablero de donde se sacaran las dimensiones para generar la posicion random
- Salida -> Entregara la coordenada y que se genera random para la computadora
|#
(define posYCp
  (lambda (seed board)
    (if(null? board) #f
       (car(getListaRandom 1 (+ 76543 seed) (+ (car(cdr(car(reverse board)))) 1) ))
       )
    )
  )

#| Funcion que se encarga de verificar si la coordenada actual es igual a la que se esta buscando para colocar el barco 
- Entrada -> * x e y : Son las coordenas donde se enceuntran creando el tablero, las cuales sirven para verificar en la posicion
               correcta donde se tiene que colocar le barco
             * posBarcos : Esta es una lista que contiene las dos coordenadas tanto (x , y) las cuales representan las posiciones
                  aleatorias que deben ser iguales a las actuales para poder proceder a colocar el barco
- Salida -> Entragara un valor booleano que representara si las coordenadas son iguales o no
- En esta funcion se ocupa recursion de cola, ya que se va llamando a la misma funcion cada ves que la condicion puesta no se cumple
  y pasando los mismo parametros excepto de la posBarcos, que se va cortando el primer elemento.
|#
(define verIgualesBa
  (lambda ( x y posBarcos)
    (if (null? posBarcos) #f
        (if (and (= x (puntoXB posBarcos)) (= y (puntoYB posBarcos))) #t
            (verIgualesBa x y (cdr posBarcos))
            )
        )
    )
  )

#| Funcion que se encarga de buscar si la coordenada actual, se encuentra ubicado algun barco
- Entrada -> * x e y : Son las coordenas donde se enceuntran creando el tablero, las cuales sirven para verificar en la posicion
                  correcta donde se tiene que colocar le barco
             *posBarcos : Esta es una lista que contiene las dos coordenadas tanto (x , y) las cuales representan las posiciones
                   aleatorias que salieron para colocar el barco de la computadora
- Salida -> Entragara un #f si el barco no se encuentra en ninguna de las posiciones, en caso contrario entregara el barco que se
        encuentra en dicha posicion.
- En esta funion se ocupa recursion de cola, ya que no se van dejando estados pendientes y al mismo tiempo se van actualizando los
 paramentros con la respuesta que dio la recursion anterior.
|#
(define verIgualesBar
  (lambda ( x y posBarcos)
    (let ((barco (barcoDeLista posBarcos)))
      (if (null? posBarcos) #f
          (if(and(numero x) (numero y))
             (if (and (= x (puntoXB posBarcos)) (= y (puntoYB posBarcos))) barco
                 (verIgualesBar x y (cdr posBarcos))
                 )
             #f
             )
          )
      )
    )
  )

#|
- Funcino que se encarga de transformar el tablero en forma de string
- Entrada-> * board: Tablero que se va a transformar en string
            * showCompelte: Es el parametro que indicara segun su valor si se muestra el tablero completo o solo la mitad del usuario
- Salida-> La salida sera el tablero en forma de string
|#
(define board->string
  (lambda (board showComplete)
    (if (and (listaDeBarco board) (numero showComplete))
        (if(= 1 showComplete)
           (boardStringComplete board)
           (boardStringNoComplete board)
           )
        #f
        )
    )
  )
           
#|
- Funcion que se encarga de transformar el tablero a forma de string y mostrara el tablero completo.
-Entrada -> tablero: El tablero que se quiere transformar a string
-Salida -> Entrega el tablero transformado a string
- En esta funcion se ocua recursion de cola, con el fin de ir recorriendo cada una de las posiciones del tablero y poder ir
 concatenando lo que se encuentra en el tablero.
|#    
(define boardStringComplete
  (lambda(tablero)
    (if(listaDeBarco tablero)
       (let recur ( (board tablero) (string "") (contador 0) (filas (puntoXB(reverse tablero))))
         (if (null? board) string
             (if (= contador filas)
                 (recur (cdr board) (string-append string(string-append (list->string (list (barcoDeLista board)))  " "  "\n")) 0 filas )
                 (recur (cdr board) (string-append string(string-append (list->string (list (barcoDeLista board)))  " " )) (+ 1 contador) filas)
                 )
             )
         )
       )
    )
  )

#|
- Funcion que representa el tablero en formato de json
- Entradas -> tablero: Tablero el cual se va a representar en forma de json
- Salida -> Entrega el tablero en formato de json, el cual se puede validar en la paginas que es el correcto
- En esta funcion ocupamos recursion de cola, ya que no vamos dejando estados pendientes, ademas de ir entregando el
    resultado como el nuevo parametro al llamado recursivo.
|#
(define board->json
  (lambda (tablero)
      (if(listaDeBarco tablero)
         (let recur ((board tablero) (string " " ) (contador 0) (filas (puntoXB(reverse tablero))) (stop -1))
           (if (null? board ) string
               (if (= stop -1)
                   (recur board (string-append string "{" "\" " "board" "\" " ":" "{") contador filas (+ stop 1))
                   (if (= 0 contador)
                       (recur (cdr board) (string-append string (string-append "\" ""fila" (number->string (car (car board)) ) "\" ") ":" "\" "(list->string (list (barcoDeLista board))) ) (+ contador 1) filas stop)
                       (if (= contador filas)
                           (if (= filas stop)
                               (recur (cdr board) (string-append string (string-append (list->string (list (barcoDeLista board))) ) "\" " "}}") 0 filas (+ 1 stop))
                               (recur (cdr board) (string-append string (string-append (list->string (list (barcoDeLista board))) )"\" ""," ) 0 filas (+ 1 stop))
                               )
                           (recur (cdr board) (string-append string (string-append (list->string (list (barcoDeLista board))))) (+ contador 1) filas stop)
                           )
                       )
                   )
               )
           )
         )
    )
  )

#|
- Funcion que entrega en tablero en formato de xml
- Entrada -> Tablero: El tablero el cual se va a representar en formato de xml
- Salida -> Entrega el tablero en formato en xml, el cual se puede verificar en paginas si esta bien aplicado el concepto,
- En esta funcion ocupamos recursion de cola, ya que no se van dejando estados pendientes y como anteriormente se ha mencionado
  el resultado que se va obteniendo se va entregando como nuevo parametro hasta que se cumple el caso base.
|#
(define board->xml
  (lambda (tablero)
      (if(listaDeBarco tablero)
         (let recur ((board tablero) (string " " ) (contador 0) (filas (puntoXB(reverse tablero))) (stop -1))
           (if (null? board ) string
               (if (= stop -1)
                   (recur board (string-append string "<board>") contador filas (+ stop 1))
                   (if (= 0 contador)
                       (recur (cdr board) (string-append string (string-append "<fila" (number->string (car (car board)) ) ">" ) (list->string (list (barcoDeLista board))) ) (+ contador 1) filas stop)
                       (if (= contador filas)
                           (if (= filas stop)
                               (recur (cdr board) (string-append string (string-append (list->string (list (barcoDeLista board))) ) "</fila" (number->string (car (car board)) ) "></board>") 0 filas (+ 1 stop))
                               (recur (cdr board) (string-append string (string-append (list->string (list (barcoDeLista board))) ) "</fila" (number->string (car (car board)) ) ">" ) 0 filas (+ 1 stop))
                               )
                           (recur (cdr board) (string-append string (string-append (list->string (list (barcoDeLista board))))) (+ contador 1) filas stop)
                           )
                       )
                   )
               )
           )
         )
    )
  )


      

#|
- Funcion que se encarga de transformar el tablero a forma de string y mostrara solo la parte del usuario
-Entrada -> tablero: El tablero que se quiere transformar a string
-Salida -> Entrega el tablero transformado a string
- En esta funcion se ocua recursion de cola, con el fin de ir recorriendo cada una de las posiciones del tablero y poder ir
 concatenando lo que se encuentra en el tablero.
|#  
(define boardStringNoComplete
  (lambda(tablero)
    (if(listaDeBarco tablero)
       (let recur ( (board tablero) (string "") (contador 0) (filas (puntoXB(reverse tablero))) (contadorO 0) (parar (+ 1(puntoYB(reverse tablero)) )))
         (if (null? board) string
             (if (= contador filas)
                 (if (>= contadorO parar) 
                     (recur (cdr board) (string-append string(string-append (list->string (list (barcoDeLista board))) " " "\n")) 0 filas (+ 2 contadorO) parar) 
                     (recur (cdr board) (string-append string(string-append (list->string (list #\- )) " " "\n")) 0 filas (+ 2 contadorO) parar)
                     )
                 (if (>= contadorO parar) 
                     (recur (cdr board) (string-append string(string-append (list->string (list (barcoDeLista board)))  " " )) (+ 1 contador) filas contadorO parar)
                     (if(eqv? (barcoDeLista board) #\x)
                        (recur (cdr board) (string-append string(string-append (list->string (list (barcoDeLista board)))  " " )) (+ 1 contador) filas contadorO parar)
                        (recur (cdr board) (string-append string(string-append (list->string (list #\- ))  " " )) (+ 1 contador) filas contadorO parar))
                     )
                 )
             )
         )
       )
    )
  )

#|
- Funcion que entrega un barco aletorio enemigo con el cual luego se ocupara para poder realizar el ataque.
- Entrada-> * tablero: Tablero de donde se sacara el barco enemigo aletoriamente para que realize el ataque
            * seed: Semilla con la cual se va ir actulizando el valor aletorio que saldra
- Salida-> Entregara el barco que salio al azar para que realize el ataque
- En esta funcion se ocupa recursion de cola, ya que se van actulizando los parametro pero no se van dejando estados pendientes
|#
(define barcoAtackOpe
  (lambda (tablero seed)
    (let recur ((barco '())(board (barcosOpenente tablero)) (x (car(getListaRandom 1 (+ seed 78) (length (barcosOpenente tablero))))) )
      (if (and (numero seed)(listaDeBarcoB tablero))
          (if (null? board) (car barco)
              (recur (append barco (list (list-ref board x) )) barco x)
              )
          #f
          )
      )
    )
  )

#|
- Funcion que entrega una lista solamente con los barcos que estan en la parte enemiga del tablero, es decir, solamente los barcos
   enemigos
- Entrada -> tablero: De donde se sacaran los barcos del enemigo para pdoer crear la lista de ellso
- Salida-> Entregara la lista con los barcos que se encuentra compuesta la parte enemiga del tablero
- En esta funcion se ocupa recursion de cola, ya que se van actulizando los parametro al vovler a llamar a al funcion, pero no se van
  dejando estados pendientes o por evaluar.
|#
(define barcosOpenente
  (lambda (tablero)
   (let recur ( (board (list-tail (reverse tablero) (mitadTablero tablero)) ) (listaBarcosOp '()) )
     (if (null? board) listaBarcosOp
         (if (or (eqv? (barcoDeLista board) #\-) (eqv? (barcoDeLista board) #\x) )
             (recur (cdr board) listaBarcosOp)
             (recur (cdr board) (append listaBarcosOp (list (barcoDeLista board))))
             )
         )
     )
    )
  )

#|
- Esta funcion se encarga de entragar la cantidad de espacios que ocupa hasta poder partir el tablero por la mitad, esta funcion se creo
con un fin en especifico que es para poder ocuparla al momento de buscar los barcos enemigos.
- Entrada-> board: Tablero de donde se sacaran las coordendas N y M, apra poder realizar el calculo
- Salida -> Entragara un valor que sera el numero de casillas que se tiene que recorrer para poder llegar a la mitad del tablero.
|#
(define mitadTablero
  (lambda (board)
    (let ( (board board) (cantidad ( * (/ (+ (puntoXB (reverse board)) 1 ) 2 ) (+ (puntoYB (reverse board)) 1 ) )) ) 
      (if (null? board) cantidad
          (if (listaDeBarco board)
              cantidad
              #f
              )
          )
      )
    )
  )

#| ----------- ** MENU ** ---------|#

#| 1. Defininmos los barcos |#

(define barcos (list #\c #\y #\k ))
;(display barcos)

#| 2. Agregamos posiciones a los barcos, ademas de agregarlos a un define "barcos"|#

(define posicionesDeBarcos (agregarBarcos barcos 10 10 '() 221218))
;(display posicionesDeBarcos)

#| 3. Verificamos las coordenadas de los barcos|#

(define barco (makeBarcos posicionesDeBarcos 10 10 '() 221218))
;(display barco)

#| 4. Creamos el tablero con las posiciones de los barcos de la computadora|#

(define board (createBoardRL 10 10 barco))
;(define board (createBoardRC 10 10 barco))
;(display board)

#| 5. luego verificamos ue el tablero este correctamente creado|#

(define check (checkBoard board))
;(display check)

#| 6. Luego hacemos algunas de las funciones que nos permite la aplicacion |#

;     * Funcion putShip

(define boardListo (putShip (putShip (putShip board '(6 6) #\h) '(9 7) #\t) '(6 9) #\o))
;(display boardListo)

;     * Funcion play

(define final (play boardListo #\h '(0 1) 156432))
;(display final)

;(display "\n")

;     * Funcion board->string

(define boardString (board->string (car final) 0))
;(display boardString)

;(display "\n")

;     * Funcion que muestran en la matriz en otros formatos

(define Board->json (board->xml board ))
(define Board->xml (board->xml (car final) ))

;(display Board->json)
;(display "\n")
;(display Board->xml)