-- Practica 1: Introducción a haskell

-- Integrantes: Luis Juárez Erick
--              Herrera Avalos Julio Alejandro
--              Peña Villegas Diego Eduardo

-- 4.1. Tipos de datos Algebraicos

--Se define el tipo de dato Shape que representa las figuras geometricas
data Shape = Circle Float
           | Square Float
           | Rectangle Float Float
           | Triangle Float
           | Trapeze Float Float Float
           deriving (Show, Eq)

--Funcion la cual calcula el area de las figuras geometricas
area :: Shape -> Float
area (Circle xs) = pi * xs * xs
area (Square xs) = xs * xs
area (Rectangle xs ys) = xs * ys
area (Triangle xs) = (sqrt 3/4) * xs * xs
area (Trapeze xs ys zs) = ((xs + ys)* zs)/2

--Funcion la cual calcula el perimetro de alguna figura geometrica
perimeter :: Shape -> Float
perimeter (Circle xs) = 2 * pi * xs
perimeter (Square xs) = 4 * xs
perimeter (Rectangle xs ys) = 2 * (xs + ys)
perimeter (Triangle xs) = 3 * xs
perimeter (Trapeze xs ys zs) = xs + ys + 2 * zs

--Se define la instancia de Ord para comparar figuras por su area
instance Ord Shape where 
    compare xs ys = compare (area xs) (area ys)


-- 4.2. Listas y Funciones