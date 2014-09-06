type Cadena = [Char]
type Pollo = (Cadena, Integer, Integer, [Cadena])

ginger = ("ginger",8968410, 150, ["kung fu", "tai chi chuan"])
rocky = ("rocky",8968300, 300, ["karate"])
anunaki = ("anunaki",8968888, 340, ["karate","muay thay","taekwondo"])

listaPollos = [ginger, rocky, anunaki]

cuadrado x =  x * x 
darPeso (_, _, peso, _) = peso
darNombre (nombre, _, _, _) = nombre
darDias (_, dias, _, _) = fromIntegral dias
darListaTecnicas (_, _, _, lista) = lista
--fromIntegral es para pasar de Integer a Double (Para poder hacer la division)
esSinFondo polloEspacial = even ((length . darNombre ) polloEspacial)

--1) edad/1 
edad :: Pollo -> Double
edad polloEspacial =  (darDias polloEspacial) / ((cuadrado 365) * pi)

--2) esAdulto/1
esAdulto :: Pollo -> Bool
esAdulto polloEspacial = ( (>5) . edad ) polloEspacial

--3) esJoven/1
esJoven :: Pollo -> Bool
esJoven polloEspacial = not (esAdulto polloEspacial)

--4) estaDesnutrido/1
estaDesnutrido :: Pollo -> Bool
estaDesnutrido polloEspacial | (esJoven polloEspacial) && (( (<50) . darPeso) polloEspacial) = True
			                       | (esAdulto polloEspacial) && (( (<200) . darPeso) polloEspacial) = True
			                       | esSinFondo polloEspacial = True
			                       | otherwise = False
--5) engordar/2
engordar :: Integer -> Pollo -> Pollo
engordar alpiste (nombre, dias, peso, lista) = (nombre, dias, peso + alpiste, lista) 

--6) alimentar/2
alimentar :: Integer -> Pollo -> Pollo
alimentar alpiste polloEspacial | estaDesnutrido polloEspacial = engordar alpiste polloEspacial
							                	| esAdulto polloEspacial = engordar (div alpiste 2) polloEspacial
							                 	| otherwise = polloEspacial

------------------------------------- Parte 2 -----------------------------------------------------
type Bigotes = Integer
type Peso = Integer
type Altura = Integer
type Raton = (Altura, Peso, Bigotes)

noSabeTieneQueAprender arteMarcial (nombre, dias, peso, lista) = not (elem arteMarcial lista)
enseniar arteMarcial (nombre, dias, peso, lista) = (nombre, dias, peso, lista ++ [arteMarcial])
agregarTitulo titulo (nombre, dias, peso, lista) = (titulo ++ nombre, dias, peso, lista)
--1)
arguiniano :: Pollo -> Pollo
arguiniano = (engordar 100)
miyagi pollo | noSabeTieneQueAprender "karate" pollo =  enseniar "karate" pollo
			       | otherwise = pollo
marcelito :: Pollo -> Pollo
marcelito (nombre, dias, peso, lista) = (nombre, dias, peso, [])

brujaTapita :: Raton -> Pollo -> Pollo
brujaTapita (alturaRaton, pesoRaton, bigotes) pollo = engordar ((alturaRaton * pesoRaton) - bigotes) pollo 

marioBross :: Cadena -> Pollo -> Pollo
marioBross arteMarcial pollo | noSabeTieneQueAprender arteMarcial pollo = ((agregarTitulo "super mario ").(enseniar arteMarcial).(enseniar "saltar")) pollo
							               | otherwise = pollo 
marcenano :: Pollo -> Pollo
marcenano pollo = (arguiniano . marcelito) pollo

--2
--planetas 
arrakis = (arguiniano, [rocky , ginger])
endor = (marcelito, [anunaki])
filtrarAdultos (entrenador, listaPollos) = filter esAdulto listaPollos
sabePoco pollo = ( (length . darListaTecnicas) pollo ) < 2
esDebil (entrenador, lista) = all sabePoco (filtrarAdultos (entrenador, listaPollos))

entrenar (entrenador, listaPollos) = map entrenador listaPollos
-- esta version de entrenar solo sirve para entrenadores que toman 1 pollo como argumento.

trainerList = [marcelito, arguiniano, marcenano]
aplicar f x = f x 
aplicarAlRevez = flip aplicar
viajeEspiritual pollo listaEntrenadores = foldl (aplicarAlRevez) pollo listaEntrenadores



--infinito arteMarcial = (arteMarcial :infinito arteMarcial) 
--nivelKarate (nombre,diasAstronomicos,peso,arteMarcial) = (nombre,diasAstronomicos,peso,arteMarcial++["karate"]) 
--armarListaInfinita (nombre,diasAstronomicos,peso,arteMarcial) "chikenNorris",90000,100,infinito (head arteMarcial))


karateInfinito = ["karate"++(show x)| x <- [1..]]
naturales = [x | x <- [1..]]
chickenNorris = ("chickenNorris", 90000000,100,karateInfinito)
