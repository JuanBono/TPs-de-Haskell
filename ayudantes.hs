type Palabra = [Char]
type Habilidad = (Palabra, Int)
type Ayudante = (Palabra, [Habilidad])
ayudantes =[ 
	("martina",[("ordenSuperior",6),("expresionLambda",7),("listasPorComprension",8)]),
	("juan", [("aplicacionParcial",9), ("listasPorComprension",6), ("sinonimosDeTipos",7)]),
	("sharon", [("currificación",5), ("aplicacionParcial",8), ("tuplas",9), ("ordenSuperior",8)])
	]

martina = ("martina",[("ordenSuperior",6),("expresionLambda",7),("listasPorComprension",8)])

-- Funciones Auxiliares --
-- conocimientos da la lista con los nombres de los conocimientos de un ayudante 
conocimientos ayudante = map fst (snd ayudante)
--sabeTema devuelve True si un ayudante conoce un tema dado. False si no lo conoce.
sabeTema tema ayudante = any (==tema) (conocimientos ayudante)

--a)
cuantosSaben :: Palabra -> [Ayudante]->Int
cuantosSaben tema listaAyudantes = (length.(filter (sabeTema tema))) listaAyudantes 

--b) Saber si los puntajes de los conocimientos van en forma creciente
-- Si los puntajes van en forma creciente, entonces al hacer una lista con los puntajes,
-- estos deberian estar ordenados de menor a mayor.
estaOrdenado [a] = True
estaOrdenado (x:xs) = ( x < (head xs) ) && (estaOrdenado xs)

puntajes ayudante = map snd (snd ayudante)

puntajeCreciente ayudante = (estaOrdenado . puntajes) ayudante 


--2) Jueces
promedio1 lista = foldl (aplicar ) 0
promedio lista = fromIntegral (sum lista) / fromIntegral (length lista) 
juezFranco ayudante = (promedio . puntajes) ayudante 


juezMaithe ayudante | sabeTema "ordenSuperior" ayudante = 9.0
	            | otherwise = 5.0

juezHernan ayudante = (fromIntegral .length . conocimientos) ayudante   

--a) Conjunto de puntajes que le dieron los jurados a un ayudante.

aplicar f x = f x 
aplicarAlRevez = flip aplicar
jueces = [juezFranco, juezMaithe, juezHernan]
conjuntoPuntajes ayudante = map (aplicarAlRevez ayudante) jueces

--b) Sumatoria de los puntajes de un ayudante 
sumPuntaje ayudante = (sum . conjuntoPuntajes) ayudante

--c) Todos los jueces lo puntuaron con mas de 7.
esBuenAyudante ayudante = all (>7) (conjuntoPuntajes ayudante) 
--d) Mostrar ejemplo de invocacion (?) del conjunto de puntajes pero con un 4to
-- representado con una expresion lambda.

lambda = (\ayudante-> map (\x-> x * x) (puntajes ayudante)) 
--juezLambda
-- lambda martina 
