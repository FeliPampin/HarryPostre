module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

------------------- Punto 1A -------------------


data Postre = UnPostre {
    nombre :: String,
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

bizcocho :: Postre
bizcocho = UnPostre "Bizcocho" ["fruta", "crema"] 100 25

tarta :: Postre
tarta = UnPostre "Tarta" ["jamon"] 0 100

chocotorta :: Postre
chocotorta = UnPostre "Chocotorta" ["cafe"] 200 0

cheeseCake :: Postre
cheeseCake = UnPostre "Cheese Cake" [] 150 50

copitos :: Postre
copitos = UnPostre "Copitos" ["ddl", "choco"] 10 10

------------------- Punto 1B -------------------

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio = variarTemperatura 1 . variarPeso (-5)

immobulus :: Hechizo
immobulus postre = variarTemperatura (temperatura postre *(-1)) postre

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarSabor "concentrado" . variarPeso (-10)

diffindo :: Number -> Hechizo
diffindo disminucion postre = variarPeso (disminucion *(-1)) postre

riddikulus :: String -> Hechizo
riddikulus sabor postre = invertirSabores . agregarSabor sabor $ postre

avadaKedavra :: Hechizo
avadaKedavra postre = eliminarTodosLosSabores . variarTemperatura (temperatura postre *(-1)) $ postre

-- Funciones auxiliares

variarTemperatura :: Number -> Postre -> Postre
variarTemperatura variacion postre = postre {temperatura = temperatura postre + variacion}

variarPeso :: Number -> Postre -> Postre
variarPeso variacion postre = postre {peso = peso postre * ((100+variacion)/100)}

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre {sabores = sabores postre ++ [sabor]}

invertirSabores :: Postre -> Postre
invertirSabores postre = postre {sabores = reverse (sabores postre)}

eliminarTodosLosSabores :: Postre -> Postre
eliminarTodosLosSabores postre = postre {sabores = []}

------------------- Punto 1C -------------------

type Mesa = [Postre]

mesa1 :: Mesa
mesa1 = [bizcocho, cheeseCake]

mesa2 :: Mesa
mesa2 = [bizcocho, copitos, tarta]

mesa3 :: Mesa
mesa3 = [bizcocho, chocotorta ,copitos]

mesa4 :: Mesa
mesa4 = [cheeseCake, bizcocho, copitos]

mesa5 :: Mesa
mesa5 = [bizcocho, copitos]

estaListo :: Postre -> Bool
estaListo postre = pesaAlgo postre && tieneSabor postre && noEstaCongelado postre

pesaAlgo :: Postre -> Bool
pesaAlgo postre = peso postre >0

tieneSabor :: Postre -> Bool
tieneSabor postre = sabores postre /= []

noEstaCongelado :: Postre -> Bool
noEstaCongelado postre = temperatura postre >0

losDejaListos :: Hechizo -> Mesa -> Bool
losDejaListos hechizo = all (estaListo . hechizo)

------------------- Punto 1D -------------------

pesoPromedio :: Mesa -> Number
pesoPromedio mesa = sumarPesos mesa 0/length mesa

sumarPesos :: Mesa -> Number -> Number
sumarPesos (postre:[]) total = total + peso postre
sumarPesos (postre:siguiente) total = sumarPesos siguiente (total + peso postre)

------------------- Punto 2A -------------------

data Mago = UnMago {
    hechizos :: [Hechizo],
    horrorcruxes :: Number
} deriving (Show, Eq)

mago :: Mago
mago = UnMago [wingardiumLeviosa, avadaKedavra, diffindo 10] 0

practica :: Mago -> Hechizo -> Postre -> Mago
practica mago hechizo postre = verificarSiAumentarHororcrux hechizo postre . aprenderHechizo mago $ hechizo

aprenderHechizo :: Mago -> Hechizo -> Mago
aprenderHechizo mago hechizo = mago {hechizos = hechizos mago ++ [hechizo]}

verificarSiAumentarHororcrux :: Hechizo -> Postre -> Mago -> Mago
verificarSiAumentarHororcrux hechizo postre mago
    | quedaIgualQueAvadaKedavra hechizo postre = mago {horrorcruxes = horrorcruxes mago +1}
    | otherwise = mago

quedaIgualQueAvadaKedavra :: Hechizo -> Postre -> Bool 
quedaIgualQueAvadaKedavra hechizo postre = hechizo postre == avadaKedavra postre 

------------------- Punto 2B -------------------


mejorHechizo :: Mago -> Postre -> Hechizo
mejorHechizo mago = seleccionarMejorHechizo (hechizos mago)

seleccionarMejorHechizo :: [Hechizo] -> Postre -> Hechizo
seleccionarMejorHechizo (hechizo1:[]) postre = hechizo1
seleccionarMejorHechizo (hechizo1:hechizo2:[]) postre = cualEsMejor hechizo1 hechizo2 postre
seleccionarMejorHechizo (hechizo1:hechizo2:siguiente) postre = cualEsMejor hechizo1 hechizo2 postre

cualEsMejor :: Hechizo -> Hechizo -> Postre -> Hechizo
cualEsMejor hechizo1 hechizo2 postre
    | cantidadSabores (hechizo1 postre) > cantidadSabores (hechizo2 postre) = hechizo1
    | otherwise = hechizo2

cantidadSabores :: Postre -> Number
cantidadSabores postre = length (sabores postre)

------------------- Punto 3A -------------------

mesaInfinita :: Mesa -> Mesa
mesaInfinita mesa = mesaInfinita (mesa ++ [chocotorta])

mesa10 :: Mesa 
mesa10 = (copitos : mesaInfinita mesa10)

