--TP Nutricionista
type Palabra = [Char]
type Sexo = Char
type Edad = Integer
type Peso = Integer
type Altura = Integer
type Contextura = Palabra
type Caracteristicas = (Peso, Altura, Contextura)
type Enfermedades = [Palabra]
type Deportes = [Palabra]
type Ocupaciones = [Palabra]
type Persona = (Palabra, Edad, Sexo, Caracteristicas, Enfermedades, Deportes, Ocupaciones)

mariano :: Persona
leandro :: Persona
gisella :: Persona
mara :: Persona
osvaldo :: Persona

mariano = ("Mariano", 20, 'M', (68, 169, "Mediana"), [], ["basket"], ["estudiante"] ) 
leandro = ("Leandro", 12, 'M', (38, 123, "Mediana"), ["alergias"], ["futbol", "skate","taekwondo"], ["estudiante"] ) 
gisella = ("Gisella", 30, 'F', (59, 162, "Pequenia"), [], ["pilates"], ["ama de casa","maestra"] ) 
mara = ("Mara", 42, 'F', (88, 195, "Grande"), ["diabetes","alergias"], [],["abogada", "profesora"] ) 
osvaldo = ("Osvaldo", 25, 'M', (98, 165, "Grande"), [], [], ["empleado"] )

listaPersonas = [mariano, leandro, gisella, mara, osvaldo]

------------ Funciones Auxiliares ---------------------------------------------
darNombre (nombre, _, _, _, _, _,_) = nombre
darEdad (_, edad, _, _, _, _,_) = edad
darSexo (_, _, sexo, _, _, _, _) = sexo
darCaracteristicas (_, _, _, caracteristicas, _, _, _) = caracteristicas
primero (x, _, _) = x
segundo (_, y, _) = y
tercero (_, _, z) = z
darPeso persona = (primero . darCaracteristicas) persona
darAltura persona = (segundo . darCaracteristicas) persona
darContextura persona = (tercero . darCaracteristicas) persona
darEnfermedades (_, _, _, _, enfermedades, _, _) = enfermedades
darDeportes (_, _, _, _, _, deportes, _) = deportes
darOcupaciones (_, _, _, _, _, _, ocupaciones) = ocupaciones
cuadrado x = x * x
infixr 2 `xor`  -- same precedence & associativity as (||)
xor :: Bool -> Bool -> Bool
xor True p = not p
xor False p = p
-------------------------------------------------------------------------------

-- 1) sedentaria/1. (No practica deportes y tiene menos de 2 ocupaciones)
sedentaria :: Persona -> Bool 
sedentaria persona = (length.darDeportes) persona < 1 && ( (length . darOcupaciones) persona) < 2 

--2) activa/1. practica al menos un deporte o tiene mas de dos ocupaciones. 
activa :: Persona -> Bool
activa persona = (length . darDeportes) persona > 0 || (length . darOcupaciones) persona > 2

--3)pesoIdeal/1. Peso Ideal = 0.75 * (altura en cm ­ - 150) + 50 
pesoIdeal :: Persona -> Double
pesoIdeal persona = 0.75 * (fromIntegral ((darAltura persona) - 150)) + 50

--4) imc/1.  El IMC se obtiene dividiendo el peso por la estatura al cuadrado
imc :: Persona -> Double
imc persona = 10000* ( fromIntegral (darPeso persona) )/ (fromIntegral ((cuadrado . darAltura) persona ))  
 --sadasdas
--5)
imcAdecuado :: Persona -> Double
imcAdecuado persona = (imc persona) + fromIntegral (div ((darEdad persona)-25) 10)  
estado :: Persona -> Palabra
estado persona | imcAdecuado persona < 20 && darEdad persona > 24 = "bajo peso"
	       | imcAdecuado persona >= 20 && imcAdecuado persona < 25 && darEdad persona > 24 = "peso normal"
	       | imcAdecuado persona >= 25 && imcAdecuado persona <= 30 && darEdad persona > 24 = "sobrepeso"
	       | imcAdecuado persona > 30 && darEdad persona > 24 = "obesidad"
	       | otherwise = "Piensen en los ninios !!!"

-----------------------------Parte 2 -------------------------------------------------------------
personasDeEjemplo = [
	("Mariano", 20, 'M', (68, 169, "Mediana"), [], ["basket"],["estudiante"] ), 
	("Leandro", 12, 'M', (38, 123, "Mediana"),["alergias"],["futbol", "skate", "taekwondo"],["estudiante"] ), 
	("Gisella", 30, 'F', (59, 162, "Pequenia"), [],["pilates"], ["ama de casa", "maestra"] ), 
	("Mara", 42, 'F', (88, 195, "Grande"),["diabetes","alergias"], [], ["abogada", "profesora"] ), 
	("Osvaldo", 25, 'M', (98, 165,"Grande"), [], [], ["empleado"])
					]

--6) personasEnEstado/1
personasEnEstado :: Palabra -> [Persona] -> [Palabra]
personasEnEstado estadoDado lista = [darNombre persona | persona <- lista, (estado persona) == estadoDado]

--7) seleccionarPersonas/2.
seleccionarPersonas :: (Persona -> Bool) -> [Persona] -> [Persona]
seleccionarPersonas funcion lista = filter funcion lista 
--7c) 
dentroDelRango :: Double -> Persona -> Bool
dentroDelRango rango persona = ( fromIntegral (darPeso persona) ) > ( (pesoIdeal persona) - rango) && fromIntegral (darPeso persona) < ( (pesoIdeal persona) + rango )

--8)
deltaPeso :: Persona -> Double
deltaPeso persona = 100 * abs (1 - (pesoIdeal persona / (fromIntegral (darPeso persona))) )
dif :: Persona -> (Palabra, Double, Double)
dif persona = (darNombre persona, pesoIdeal persona, deltaPeso persona) 
diferenciaDePeso listaPersonas = map dif listaPersonas

--9)
estasEnLaB = ["diabetes", "asma", "anemia"]

interseccion [] lista2 = []
interseccion lista1 lista2 = [x | x <- lista1, elem x lista2]

enRiesgo persona = (interseccion (darEnfermedades persona) estasEnLaB) /= [] || deltaPeso persona > 0.8 * (pesoIdeal persona)  

toqueDeMascherano (nombre, edad, sexo, caracteristicas, enfermedades, deportes, ocupaciones) = 
	(nombre, edad, sexo, caracteristicas, [] , deportes, ocupaciones) 

--enRiesgo mara

funcion = (even . cuadrado . sum)
ensenarArtes artesMarciales arte |elem arte artesMarciales = artesMarciales ++ [arte, "saltar"]
				 | otherwise = artesMarciales ++ "saltar"
