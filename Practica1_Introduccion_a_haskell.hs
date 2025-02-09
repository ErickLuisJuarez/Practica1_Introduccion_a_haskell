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

-- 3 Haskellium
data Haskellium = Haskellium {name :: String,
                              lastName1 :: String,
                              lastName2 :: String,
                              location :: Point,
                              houseShape :: Shape} deriving (Show)

--Funcion que crea un nuevo Haskellium hijo de dos padres y con un nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son father mother childName = 
    Haskellium { name = childName,
                lastName1 = lastName1 father,
                lastName2 = lastName2 mother,
                location = location father,
                houseShape = houseShape father,
               }

--Funcion que calcula las unidades necesarias para construir la casa de un Haskellium dado
houseCost :: Haskellium -> Float
houseCost haskellium = 
    let baseArea = area (houseShape haskellium) -- Área del techo
        wallArea = perimeter (houseShape haskellium) * 2.5 -- Área de las paredes
    in baseArea + wallArea

--Funcion que calucla el tiempo que le toma a un Haskellium llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork haskellium = 
    let distanceOrigin = distance (location haskellium) (Point 0 0)
    in if distanceOrigin < 300 
       then distanceOrigin / 30  -- Viaja en bicicleta
       else distanceOrigin / 70  -- Viaja en moto


-- 4.2. Listas y Funciones

-- Funcion que verifica si una cadena es un palindromo o no
isPal :: String -> Bool
isPal xs = xs == reverse xs

-- Funcion que recursivamente implementa una lista de lista y regresa la concatenación de todas las listas contenidas
concat' :: [[a]] -> [a]
concat'[] = []
concat' (x:xs) = x ++ concat' xs

-- 4.3. Árboles

-- Se define le tipo de dato para arboles OneTwoTree para representar algun arbol con uno o dos hijos
data OneTwoTree arbol = Void
                  | Node arbol (OneTwoTree arbol)
                  | Branch arbol (OneTwoTree arbol) (OneTwoTree arbol)
                  deriving (Show)

--Funcion que recursivamente calcula la suma de los elementos de un arbol
suma :: OneTwoTree Int -> Int
suma Void = 0
suma (Node x xs) = x + suma xs
suma (Branch x xs ys) = x + suma xs + suma ys

--Funcion que recursivamente determunia si un arbol no contiene el valor 0
sinCero :: OneTwoTree Int -> Bool
sinCero Void = True
sinCero (Node x xs) = x /= 0 && sinCero xs
sinCero (Branch x xs ys) = x /= 0 && sinCero xs && sinCero ys
