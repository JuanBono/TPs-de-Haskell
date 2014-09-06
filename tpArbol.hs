-- DEFINICIONES DE TIPO --
type Arbol = ([Char], Double, Double, Double)
type Especie = ([Char], Integer, Double)

laVerde :: [Arbol] 
campoLibre :: [Arbol]
fondoDeCasa :: [Arbol]

-- CONSTANTES --
--Son los arboles por separado, junto a sus caracteristicas normales. Son una 3-upla (nombre,altura,ancho)
pino = ("pino", 3.2, 1.0)
ombu = ("ombu", 4.5, 4.2)
eucalipto = ("eucalipto", 9.0, 1.5)
jacaranda = ("jacaranda", 25.0, 0.7)
cerezo = ("cerezo", 2.5, 0.4)


-- Reservas, cada 4-upla es un arbol con sus caracteristicas (nombre,altura,ancho,vitalidad)
laVerde = [
	("jacaranda", 6, 1, 1.4),
	("pino", 5, 3, 1.9),
	("eucalipto", 5, 4, 0.7),
	("jacaranda", 10, 2, 1.0),
	("cerezo", 7, 11, 0.9),
	("ombu", 8, 10, 2.1)
	]
campoLibre = [
	("pino", 0.5, 0.5, 0.5),
	("pino", 5.0, 1.0, 1.2),
	("pino", 6.0, 0.8, 1.8),
	("pino", 5.0, 1.1, 1.2),
	("pino", 5.0, 1.5, 1.1),
	("pino", 5.0, 0.9, 0.9),
	("pino", 6.0, 1.1, 1.2),
	("pino", 5.0, 1.6, 1.0)
	]

fondoDeCasa = [
	("pino", 2.0, 0.5, 2),
	("pino", 1.5, 0.5, 2),
	("eucalipto", 4.0, 1.0, 2)
	]

-- Lista con las caracteristicas normales de cada especie. Cada nombre hace referencia a las 3-uplas definidas arriba.
caracteristicasNormales = [pino, ombu, eucalipto, jacaranda, cerezo]

-- Lista con los meses del año, la primer letra esta en mayuscula, tenerlo en cuenta al llamar a las funciones.
meses = [
	"Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
	"Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
	]

-- PARTE 1 --
--a--
esFrondoso (_, altura, ancho, _) = (altura >= 6) && (altura <= 15) && (ancho > altura)
filtroFrondosos :: [Arbol] -> [Arbol]
filtroFrondosos listaArboles = filter esFrondoso listaArboles

darNombre (nombre , _, _, _) = nombre

nombresFrondosos :: [Arbol] -> [[Char]]
nombresFrondosos listaArboles = map darNombre (filtroFrondosos listaArboles)
nombresFrondosos2 listaArboles = [darNombre arbol | arbol <- listaArboles, esFrondoso arbol]

--b--
tieneBuenaVitalidad (_, _, _, vitalidad) = vitalidad > 1
todosLosFrondososSonVitales :: [Arbol] -> Bool
todosLosFrondososSonVitales listaArboles = all tieneBuenaVitalidad (filtroFrondosos listaArboles)

--c--
lluvia :: Double -> Arbol -> Arbol
lluvia milimetros (nombre, altura, ancho, vitalidad) = (nombre, altura + 1, ancho, vitalidad+(milimetros/100))
temperatura :: Double -> Arbol -> Arbol
temperatura grados (nombre, altura, ancho, vitalidad) | grados < 0 = (nombre, altura, ancho, vitalidad * 0.5)
						      | grados > 40 = (nombre, altura, ancho, vitalidad * 0.6)
						      | otherwise = (nombre, altura, ancho, vitalidad)
granizo :: Arbol -> Arbol
granizo (nombre, altura, ancho, vitalidad) = (nombre, altura / 2, ancho / 2, vitalidad)
--d--
paraTodos funcion listaArboles = map funcion listaArboles


-- PARTE 2 --
darAltura (_, altura, _) = altura

darAncho (_, _, ancho) = ancho

coincideNombre nombre (nombreArbol, _, _) = nombreArbol == nombre

-- Devuelve una arbol (tupla) cuyo nombre es igual al dado.
filtroArbol nombre = (head . filter (coincideNombre nombre)) caracteristicasNormales

-- Se ingresa un arbol y devuelve la altura normal de la especie
alturaEspecie = darAltura . filtroArbol

-- Se ingresa un arbol y devuelve el ancho normal de la especie
anchoEspecie = darAncho . filtroArbol

--a) sePuedeTransplantar/2. Define si un arbol se puede transplantar en un mes dado.
--	 Debe tener buena vitalidad y el mes no debe contener la letra 'r'
sePuedeTransplantar :: Arbol -> [Char] -> Bool
sePuedeTransplantar (_, _, _, vitalidad) mes = (vitalidad > 1) && 
						not (elem 'r' mes) && 
						(elem mes meses)

--b) sePuedePodar/2. Evalua si un arbol se puede transplantar en un mes dado.
--	 El arbol debe poder transplantarse en ese mes y su altura debe ser mayor a la normal.
sePuedePodar :: Arbol -> [Char] -> Bool
sePuedePodar (nombre, altura, ancho, vitalidad) mes = (sePuedeTransplantar (nombre, altura, ancho, vitalidad) mes) 
							&& (altura > alturaEspecie nombre)

--c biomasa/1. Estima la biomasa de una reserva dada.
biomasa::  [Arbol]-> Double
biomasa listaArboles = sum ( map biomasaPorArbol listaArboles)

biomasaPorArbol :: Arbol -> Double
biomasaPorArbol (_, altura, ancho, vitalidad)= pi * ancho * altura * vitalidad

--d) Evalua el tamaño de un arbol. 
gigante :: Arbol -> Bool
gigante (nombre, altura, ancho, vitalidad) = altura > (alturaEspecie nombre) && ancho > (anchoEspecie nombre)
normal :: Arbol -> Bool
normal (nombre, altura, ancho, vitalidad) = altura == (alturaEspecie nombre) && ancho == (anchoEspecie nombre)
enano :: Arbol -> Bool
enano (nombre, altura, ancho, vitalidad) = altura < (alturaEspecie nombre) && ancho < (anchoEspecie nombre)

--e) sonTodos/2. Pasa una funcion como parametro y evalua si todos en la reserva la cumplen.
sonTodos :: (Arbol -> Bool) -> [Arbol] -> Bool
sonTodos funcion listaArboles = all funcion listaArboles

--f) listaPoda/2. Obtener la lista con los árboles a podar en un mes dado de una reserva.
listaPoda :: [Arbol] -> [Char] -> [Arbol]
listaPoda reserva mes = [arbol | arbol <- reserva, sePuedePodar arbol mes]

--g) sePuedeMoverLaReserva/2. Evaluar si una reserva se puede mudar, 
--	 para ello es necesario que todos los árboles se puedan transplantar en el mes consultado.
sePuedeMoverLaReserva :: [Arbol] -> [Char] -> Bool
sePuedeMoverLaReserva reserva mes = all (flip sePuedeTransplantar mes) reserva

--h) biomasaPoda/2. Crear una función que realice la poda en los árboles aptos y recalcule 
--   la biomasa de la reserva luego de realizar la poda en todos los árboles que fué posible
--   realizarla. Nota: Cuando se realiza la poda a un árbol, la biomasa se reduce en un 25%.
biomasaPoda :: [Arbol] -> [Char] -> Double
biomasaPoda listaArboles mes = 0.75 * ((biomasa . listaPoda listaArboles) mes)
