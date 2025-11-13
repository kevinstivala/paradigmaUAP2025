module Clase2 exposing (..)
import Html exposing (th)
import List exposing (concat)
import Html exposing (p)
import List exposing (isEmpty)


head : List a -> a
head list =
    Maybe.withDefault (Debug.todo "head called on empty list") (List.head list)


tail : List a -> List a
tail list =
    Maybe.withDefault [] (List.tail list)


isEmpty : List a -> Bool
isEmpty list =
    List.isEmpty list


{-| Ejercicios de Programación Funcional - Clase 2
Este módulo contiene ejercicios para practicar conceptos de programación funcional en Elm.
Cada función debe implementarse usando principios de programación funcional.

Nota: Las funciones que podrían fallar (como buscar elementos) devuelven valores por defecto (0)
en lugar de usar Maybe. Trabajamos con List en lugar del List de Scala.

-}



-- Concatenar
-- Dadas 2 listas, devuelve una lista que es la concatenación de ambas.
-- Usar recursión


concatenar : List Int -> List Int -> List Int
concatenar lista1 lista2 =
    case lista1 of
        [] -> lista2
        x :: xs ->
            x :: concatenar xs lista2


-- Buscar
-- Dada una lista y una función de comparación, devuelve el valor que cumple la condición.
-- Devuelve 0 si la lista está vacía.


buscar : List Int -> (Int -> Int -> Bool) -> Int
buscar lista com =
    case lista of
        [] -> 0
        [x] -> x
        x :: y :: xs ->
            if com y x then
                buscar (y :: xs) com
            else
                x


-- Busca el Máximo
-- Encuentra el valor máximo en una lista


max : List Int -> Int
max lista =
    case lista of
        [] -> 0
        [x] -> x
        x :: xs ->
            let
                maxRest = max xs
            in
            if x > maxRest then
                x
            else
                maxRest

--[ 1, 5, 3, 2, 4 ] = 5
-- 1 > 5
-- 5 > 4
-- 3 > 4
-- 2 > 4 = 4
-- 4


-- Busca el Mínimo
-- Encuentra el valor mínimo en una lista


min : List Int -> Int
min lista =
    case lista of
        [] -> 0
        [x] -> x
        x :: xs ->
            let
                minRest = min xs
            in
            if x < minRest then
                x
            else
                minRest



-- Filtra la lista de valores mayores que el valor e pasado por parámetro


maximos : List Int -> Int -> List Int
maximos lista e =
    case lista of
        [] -> []
        h :: t ->
            if h > e then
                h :: maximos t e
            else
                maximos t e
            



-- Filtra la lista de valores menores que el valor e pasado por parámetro


minimos : List Int -> Int -> List Int
minimos lista e =
    case lista of
        [] -> []
        h :: t ->
            if h < e then
                h :: minimos t e
            else
                minimos t e



-- Ordena los valores de una lista utilizando quicksort


quickSort : List Int -> List Int
quickSort xs =
    case xs of
        [] ->
            []

        pivot :: resto ->
            -- TODO: Implementar quicksort recursivamente
            -- 1. Dividir resto en menores y mayores que pivot
            -- 2. Ordenar recursivamente ambas particiones
            -- 3. Concatenar: (menores ordenados) ++ [pivot] ++ (mayores ordenados)
            let
                restosMin = quickSort (minimos resto pivot)
                iguales = List.filter (\x -> x == pivot) resto
                restosMax = quickSort (maximos resto pivot)
            in
            restosMin ++ iguales ++ (pivot :: restosMax)


-- Obtiene un elemento en la posición n (empezando desde 0)
-- Devuelve 0 si la posición está fuera de rango


obtenerElemento : List Int -> Int -> Int
obtenerElemento lista posicion =
    case (lista, posicion) of
        ([], _) -> 0
        (xs, n) ->
            if n < 0 then
                0
            else
                case xs of
                    [] -> 0
                    x :: rest ->
                        if n == 0 then
                            x
                        else
                            obtenerElemento rest (n - 1)



-- Busca la mediana
-- En el ámbito de la estadística, la mediana representa el
-- valor de la variable de posición central en un conjunto de datos ordenados.
-- Devuelve 0 si la lista está vacía.


mediana : List Int -> Int
mediana lista =
    let
        ordenada = quickSort lista
        n = List.length ordenada
    in
    case ordenada of
        [] -> 0
        [x] -> x
        _ ->
            if modBy 2 n == 1 then
                obtenerElemento ordenada (n // 2)
            else
                let
                    a = obtenerElemento ordenada ((n // 2) - 1)
                    b = obtenerElemento ordenada (n // 2)
                in
                (a + b) // 2




-- Cuenta los elementos


contar : List Int -> Int
contar lista =
    case lista of
        [] -> 0
        _ :: xs ->
            1 + contar xs




-- Acumula los elementos


acc : List Int -> Int
acc lista =
    case lista of
        [] -> 0
        x :: xs ->
            x + acc xs



-- Filtra los elementos de la lista xs según la función p


filtrar : List Int -> (Int -> Bool) -> List Int
filtrar xs p =
    case xs of
        [] -> []
        h :: t ->
            if p h then
                h :: filtrar t p
            else
                filtrar t p


-- Filtra los elementos pares usando la función filtrar


filtrarPares : List Int -> List Int
filtrarPares xs =
    -- Pista: Usar modBy 2 para verificar números pares
    case xs of
        [] -> []
        h :: t ->
            if modBy 2 h == 0 then
                h :: filtrarPares t
            else
                filtrarPares t




-- Filtra los elementos múltiplos de 3 usando filtrar


filtrarMultiplosDeTres : List Int -> List Int
filtrarMultiplosDeTres xs =
    case xs of
        [] -> []
        h :: t ->
            if modBy 3 h == 0 then
                h :: filtrarMultiplosDeTres t
            else
                filtrarMultiplosDeTres t



-- Acumula los elementos aplicándoles fx


acumular : List Int -> (Int -> Int) -> Int
acumular lista fx =
    case lista of
        [] -> 0
        x :: xs ->
            fx x + acumular xs fx



-- Acumula todos los elementos de una lista usando acumular (función identidad)


acumularUnidad : List Int -> Int
acumularUnidad lista =
    -- Pista: (\x -> x)
    case lista of
        [] -> 0
        x :: xs ->
            x + acumularUnidad xs




-- Acumula el doble de los elementos de una lista usando acumular


acumularDoble : List Int -> Int
acumularDoble lista =
    -- Pista: (\x -> x * 2)
    case lista of
        [] -> 0
        x :: xs ->
            x * 2 + acumularDoble xs



-- Acumula el cuadrado de los elementos de una lista usando acumular


acumularCuadrado : List Int -> Int
acumularCuadrado lista =
    -- Pista: (\x -> x * x)
    case lista of
        [] -> 0
        x :: xs ->
            x * x + acumularCuadrado xs



-- Transforma la lista a una lista de otro tipo
-- Esto es equivalente a la función map de Scala


transformar : List Int -> (Int -> a) -> List a
transformar lista fx =
    case lista of
        [] -> []
        x :: xs ->
            fx x :: transformar xs fx



-- Retorna true si un elemento existe en la lista


existe : List Int -> Int -> Bool
existe lista nro =
    case lista of
        [] -> False
        x :: xs ->
            if nro == x then
                True
            else
                existe xs nro



-- Une 2 listas pasadas por parámetros pero ignora los repetidos


unirOfSet : List Int -> List Int -> List Int
unirOfSet lista otraLista =
    -- Vas a necesitar una función auxiliar para remover duplicados
    removerDuplicados(
    case lista of
        [] -> removerDuplicados otraLista
        x :: xs ->
            if existe otraLista x then
                unirOfSet xs otraLista
            else
                x :: unirOfSet xs otraLista
    )


unirOfSet2 : List Int -> List Int -> List Int
unirOfSet2 lista otraLista =
    -- Vas a necesitar una función auxiliar para remover duplicados
    removerDuplicados (concatenar lista otraLista)



-- Función auxiliar para remover duplicados de una lista


removerDuplicados : List Int -> List Int
removerDuplicados lista =
    case lista of
        [] -> []
        x :: xs ->
            if existe xs x then
                removerDuplicados xs
            else
                x :: removerDuplicados xs


-- OPCIONAL: Subconjuntos
-- Dada una lista de enteros, retorna una lista con todos los posibles subconjuntos
-- Por ejemplo: [1,2,3] -> [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]


subSets : List Int -> List (List Int)
subSets lista =
    case lista of
        [] ->
            [ [] ]
        x :: xs ->
            let
                subconjuntosSinX = subSets xs
                subconjuntosConX = transformar subconjuntosSinX (\s -> x :: s)
            in
            subconjuntosSinX ++ subconjuntosConX



-- OPCIONAL: Cortar
-- Dada una lista de enteros y un número entero n, retorna subconjuntos con n elementos
-- Ejemplo: [1,2,3,4,5] y 2 -> [[1,2], [3,4], [5]]


cortar : List Int -> Int -> List (List Int)
cortar lista n =
    []



-- Función auxiliar para tomar los primeros n elementos de una lista


tomar : Int -> List a -> List a
tomar n lista =
    []



-- Función auxiliar para saltar los primeros n elementos de una lista


saltar : Int -> List a -> List a
saltar n lista =
    []



-- Ejemplos de uso y funciones de prueba
-- Podés usar estos para probar tus implementaciones


ejemplos : List String
ejemplos =
    [ "max [1,2,3,4,5] debería devolver 5"
    , "min [1,2,3,4,5] debería devolver 1"
    , "maximos [1,2,3,4,5] 3 debería devolver [4,5]"
    , "minimos [1,2,3,4,5] 3 debería devolver [1,2]"
    , "quickSort [3,1,4,1,5,9,2,6] debería devolver [1,1,2,3,4,5,6,9]"
    , "contar [1,2,3,4,5] debería devolver 5"
    , "acc [1,2,3,4,5] debería devolver 15"
    , "filtrarPares [1,2,3,4,5,6] debería devolver [2,4,6]"
    , "filtrarMultiplosDeTres [1,2,3,6,9,10] debería devolver [3,6,9]"
    , "acumularDoble [1,2,3] debería devolver 12 (2+4+6)"
    , "acumularCuadrado [1,2,3] debería devolver 14 (1+4+9)"
    , "unir [1,2] [3,4] debería devolver [1,2,3,4]"
    , "existe [1,2,3] 2 debería devolver True"
    , "existe [1,2,3] 4 debería devolver False"
    ]
