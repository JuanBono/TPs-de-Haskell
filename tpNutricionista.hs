type Persona = (Nombre, Edad, Sexo, Caracteristicas, [Enfermedad], [Deporte], [Ocupacion])
type Nombre = [Char]
type Edad = Int
type Sexo = Char
type Caracteristicas = (Peso, Altura, Contextura)
type Altura = Double
type Peso = Double
type Contextura = [Char]
type Enfermedad = [Char]
type Deporte = [Char]
type Ocupacion = [Char]

personasDeEjemplo :: [Persona]
personasDeEjemplo = [("Mariano", 20, 'M', (68.0, 169.0, "Mediana"), [], ["basket"], ["estudiante"]),
					("Leandro", 12, 'M', (38.0, 123.0, "Mediana"), ["alergias"], ["futbol", "skate", "taekwondo"], ["estudiante"]),
					("Gisella", 30, 'F', (59.0, 162.0, "Pequeña"), [], ["pilates"], ["ama de casa", "maestra"]),
					("Mara", 42, 'F', (88.0, 195.0, "Grande"), ["diabetes", "alergias"], [],["abogada", "profesora"]),
					("Osvaldo", 25, 'M', (98.0, 165.0, "Grande"), [], [], ["empleado"])
					]

caloriasPorTarea :: [([Char], Int)]
caloriasPorTarea = [("basket",1100), ("futbol",1100), ("skate",800), ("maestra", 320), ("estudiante", 220), ("empleado", 480)]

enfermedadesPeligrosas = ["diabetes", "asma", "anemia"]

darNombre (nombre, _, _, _, _, _, _) = nombre
darPeso (_, _, _, (peso, _, _), _, _, _) = peso
darEnfermedades (_, _, _, _, enfermedad, _, _) = enfermedad

--1)
sedentaria :: Persona -> Bool
sedentaria (_, _, _, _, _, deportes, ocupaciones) = (deportes == []) && (2 > (length ocupaciones))

--2)
activa :: Persona -> Bool
activa (_, _, _, _, _, deportes, ocupaciones) = (deportes /= []) || (2 <= (length ocupaciones))

--3)
pesoIdeal :: Persona -> Double
pesoIdeal (_, _, _, (_, altura, _), _, _, _) = 0.75 * (altura - 150) + 50

--4)
imc :: Persona -> Double
imc (_, _, _, (peso, altura, _), _, _, _) = imcDadoPesoYAltura peso altura

imcDadoPesoYAltura peso altura = 10000 * peso / (altura * altura)

--5)
estado :: Persona -> [Char]
estado (_, edad, _, (peso, altura, _), _, _, _) | edad > 24 = darEstado(round(imcDadoPesoYAltura peso altura) - (div (edad - 25) 10))
                                                | otherwise = "error"

darEstado imc | imc < 20 = "bajo peso"
	      | (20 <= imc) && (imc < 25) = "peso normal"
	      | (25 >= imc) && (imc < 30) = "sobrepeso"
	      | otherwise = "obesidad"

--6)
personasEnEstado :: [Char] -> [Persona] -> [Nombre]
personasEnEstado estadoDado listaPersonas = [darNombre persona | persona <- listaPersonas, tieneEstado estadoDado persona]

tieneEstado estadoDado persona = (estadoDado == estado persona)

--7)
--a)
seleccionarPersonas :: (Persona -> Bool) -> [Persona] -> [Nombre]
seleccionarPersonas f listaPersonas = map darNombre (filter f listaPersonas)

--b)
practicaUnDeporteDado :: Deporte -> Persona -> Bool
practicaUnDeporteDado deporte (_, _, _, _, _, listaDeportes, _) = (filter (== deporte) listaDeportes) /= []

esMujer :: Persona -> Bool
esMujer (_, _, sexo, _, _, _, _) = sexo == 'F'

--c)
dentroDelRango :: Double -> Persona -> Bool
dentroDelRango rango persona = rango > (porcentajeDeDiferencia persona)

-- Esta formula no esta bien pero no se cual sera.
porcentajeDeDiferencia persona = 100 * abs(1 - (pesoIdeal persona / darPeso persona))

--8)
diferenciaDePeso :: [Persona] -> [(Nombre, Peso, Peso, Double)]
diferenciaDePeso listaPersonas = map darNuevaTupla listaPersonas

darNuevaTupla persona = (darNombre persona, pesoIdeal persona, darPeso persona, porcentajeDeDiferencia persona)

--9)
estaEnRiesgo :: Persona -> Bool
estaEnRiesgo persona = (porcentajeDeDiferencia persona > 80) || tieneEnfermedadPeligrosa persona

tieneEnfermedadPeligrosa persona = (filter esEnfermedadPeligrosa (darEnfermedades persona)) /= []

esEnfermedadPeligrosa enfermedad = elem enfermedad enfermedadesPeligrosas

--10)
calorias :: Persona -> [([Char], Int)] -> Int
calorias (_, _, _, _, _, deportes, ocupaciones) listaTareas = darCaloriasTotal (deportes ++ ocupaciones) listaTareas

darCaloriasTotal tareasRealizadas listaTareas = sumarCalorias (calcularCalorias tareasRealizadas listaTareas) tareasRealizadas (promedioLista (map snd listaTareas))

sumarCalorias listaCalorias tareasRealizadas promedio = sum listaCalorias + cantidadTareasNoEncontradas listaCalorias tareasRealizadas * promedio

calcularCalorias tareasRealizadas listaTareas = map (darCaloriasPorTarea listaTareas) (filter (seEncuentraTarea listaTareas) tareasRealizadas)

seEncuentraTarea listaTareas tarea = elem tarea (map fst listaTareas)

darCaloriasPorTarea listaTareas tarea = snd(head(filter ((== tarea) . fst) listaTareas))

cantidadTareasNoEncontradas listaCalorias listaTareas = length listaTareas - length listaCalorias

promedioLista lista = div (sum lista) (length lista)
