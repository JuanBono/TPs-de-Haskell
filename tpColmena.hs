type TipoColmena = ([Char], [([Char], Integer)], [([Char], Integer)])

colmenas = [("colmena1", [("obrera",1100),("zangano",9)],[("miel",6),("propoleo",6)]),
	("colmena2", [("reina",1),("zangano",22),("obrera",800),("otros",30)],[("miel",6),("polen",4)]),
	("colmena3", [("reina",1),("obrera",1200),("zangano",10)],[("nectar",8),("polen",4)])]

productos = ["miel", "polen", "propoleo", "nectar"]

darNombre (nombreColmena, _, _) = nombreColmena
darProductos (_, _, listaProductos) = listaProductos
darPoblacion (_, listaPoblacion, _) = listaPoblacion

buscarColmenaPorNombre nombreColmena = buscarColmenaPorNombreEnLista nombreColmena colmenas
buscarColmenaPorNombreEnLista nombreColmena listaColmenas = head (filter ((== nombreColmena) . darNombre) listaColmenas)
darProductosPorNombre = darProductos.buscarColmenaPorNombre
darPoblacionPorNombre = darPoblacion.buscarColmenaPorNombre

--1)
huerfanas = map darNombre (filter noTieneReina colmenas)

noTieneReina (_, materialVivo, _) = all ((/= "reina") . fst) materialVivo

--2)
poblacionAceptable cantidad = map darNombre (colmenasConPoblacionAceptable cantidad colmenas)

colmenasConPoblacionAceptable cantidad colmenas = filter (tienePoblacionSuficiente cantidad) colmenas

tienePoblacionSuficiente cantidad (_, materialVivo, _) = sum (map snd materialVivo) > cantidad

--3)
buenaCosecha :: [Char] -> Integer -> [Char] -> Bool
buenaCosecha producto cantidad nombreColmena = tieneProductoSuficiente producto cantidad (darProductosPorNombre nombreColmena)

tieneProductoSuficiente producto cantidad listaProductos = any ((> cantidad) . snd) (filter ((== producto) . fst) listaProductos)

--4)
fuerte nombreColmena = tienePoblacionSuficiente 500 (buscarColmenaPorNombre nombreColmena)

--5)
pocosZanganos cantidad nombreColmena = any (tieneSuficientesZanganos cantidad) (darPoblacionPorNombre nombreColmena)

tieneSuficientesZanganos cantidad (nombreProducto, cantidadProducto) = (nombreProducto == "zangano") && (cantidad > cantidadProducto)

--6)
enjambrar :: Integer -> [TipoColmena]
enjambrar cantidad = unirColmenas (map nuevaColmena (colmenasConPoblacionAceptable cantidad (filter (not . noTieneReina) colmenas)))

nuevaColmena (nombreViejaColmena, viejaPoblacion, viejosProductos) = (nombreViejaColmena, map (nuevaCantidad 2) viejaPoblacion, map (nuevaCantidad 4) viejosProductos)

nuevaCantidad factor (nombreViejo, cantidadVieja) = (nombreViejo, div cantidadVieja factor)
 
unirColmenas nuevasColmenas = (map (verificarColmenas nuevasColmenas) colmenas) ++ (map modificarNuevaColmena nuevasColmenas)

verificarColmenas nuevasColmenas (viejoNombre, viejaPoblacion, viejosProductos) | (any ((== viejoNombre) . darNombre) nuevasColmenas) = modificarColmenas (buscarColmenaPorNombreEnLista viejoNombre nuevasColmenas) viejaPoblacion viejosProductos
										| otherwise = (viejoNombre, viejaPoblacion, viejosProductos)

modificarColmenas (nuevoNombre, nuevosPoblacion, nuevosProductos) viejaPoblacion viejosProductos = (nuevoNombre, modificarAmbasListas viejaPoblacion nuevosPoblacion, modificarAmbasListas viejosProductos nuevosProductos)

modificarAmbasListas viejaLista nuevaLista = map (modificarUnaLista nuevaLista) viejaLista

modificarUnaLista nuevaLista (viejoPrimero, viejoSegundo) = modificarValor (filter ((== viejoPrimero) . fst) nuevaLista) (viejoPrimero, viejoSegundo)

modificarValor nuevaLista (viejoPrimero, viejoSegundo)  | (nuevaLista == []) = (viejoPrimero, viejoSegundo)
							| otherwise = (viejoPrimero, viejoSegundo - snd(head(nuevaLista)))

modificarNuevaColmena (nombreColmena, poblacion, productos) = (nombreColmena ++ "b", map modificarClaseSocial poblacion, productos)

modificarClaseSocial (nombreClaseSocial, cantidadVieja) | (nombreClaseSocial == "reina") = (nombreClaseSocial, cantidadVieja + 1)
                 			                | (nombreClaseSocial == "obrera") = (nombreClaseSocial, cantidadVieja - 1)
				         	        | otherwise = (nombreClaseSocial, cantidadVieja)
--7)
