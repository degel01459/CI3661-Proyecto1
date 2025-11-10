-- Autor: Kevin Briceño
-- Carnet: 15-11661
-- src/Engine/Core.hs

module Engine.Core (procesarComando, resolverTrampasAlEntrar) where
import Engine.Types
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as M

-- util: trim local (elimina espacios al inicio y fin)
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- acciones de inventario
añadirInventario :: NombreObjeto -> Cantidad -> Inventario -> Inventario
añadirInventario idq c inv = M.insertWith (+) idq c inv

quitarInventario :: NombreObjeto -> Cantidad -> Inventario -> Inventario
quitarInventario idq c inv = case M.lookup idq inv of
  Nothing -> inv
  Just n  -> let nuevo = n - c in if nuevo <= 0 then M.delete idq inv else M.insert idq nuevo inv

-- calcula peso total según catalogo
calcularPeso :: Inventario -> ContenedorObjetos -> Int
calcularPeso inv catalogo =
  sum [ qty * objPeso o | (oid, qty) <- M.toList inv, Just o <- [M.lookup oid catalogo] ]

-- devuelve cantidad de un item en el inventario (0 si no existe)
cantidadItem :: NombreObjeto -> Inventario -> Int
cantidadItem oid inv = M.findWithDefault 0 oid inv

-- ¿tiene al menos n unidades de este item?
tieneAlMenos :: NombreObjeto -> Int -> Inventario -> Bool
tieneAlMenos oid n inv = cantidadItem oid inv >= n

-- verifica si el jugador tiene la llave de salida y los 8 tokens (o la flag mapa_completo)
puedeIntentarSalida :: EstadoJuego -> Bool
puedeIntentarSalida st =
  let inv = inventarioJugador st
      tieneLlave = cantidadItem "llave_salida" inv > 0
      mapaFlag = M.findWithDefault False "mapa_completo" (flags st)
      -- comprobación flexible: si usas monedas doradas llámalas "moneda_dorada"
      tieneMonedas = tieneAlMenos "moneda_dorada" 8 inv
  in tieneLlave && (mapaFlag || tieneMonedas)

-- buscar sala
lookupSala :: NombreSala -> ContenedorSalas -> Maybe Sala
lookupSala = M.lookup

-- Busca un NPC por nombre sin importar mayúsculas/minúsculas.
lookupNPCCaseInsensitive :: NombreNPC -> M.Map NombreNPC NPC -> Maybe (NombreNPC, NPC)
lookupNPCCaseInsensitive q mp =
  let ql = map toLower q
      matches = [ (k,v) | (k,v) <- M.toList mp, map toLower k == ql ]
  in case matches of
       (x:_) -> Just x
       []    -> Nothing

-- mostrar sala
mostrarSala :: Sala -> ContenedorObjetos -> String
mostrarSala sala catalogo =
  let desc = salaDesc sala
      salidasList = M.toList (salidasSala sala)
      salidasTxt = if null salidasList then "No hay salidas visibles."
                   else "Salidas: " ++ unwords (map (\(d, s) ->
                        let bloque = case bloqueadaPor s of
                                      Nothing -> ""
                                      Just k -> " (bloqueada por " ++ k ++ ")"
                        in mostrarDir d ++ " -> " ++ destinoSalida s ++ bloque) salidasList)
      objs = M.toList (objetosSala sala)
      objsTxt = if null objs then "No ves objetos aquí."
                else "Objetos: " ++ unwords [ o ++ (if n>1 then " x" ++ show n else "") | (o,n) <- objs ]
      npcs = M.keys (npcsSala sala)
      npcsTxt = if null npcs then "" else "Personas: " ++ unwords npcs
      traps = M.keys (trampasSala sala)
      trapsTxt = if null traps then "" else "Trampas: " ++ unwords traps
  in unlines $ filter (not . null) [desc, salidasTxt, objsTxt, npcsTxt, trapsTxt]

resolverTrampasAlEntrar :: Sala -> EstadoJuego -> (String, EstadoJuego)
resolverTrampasAlEntrar sala estado =
  let traps = M.elems (trampasSala sala)
      inv = inventarioJugador estado
      fl  = flags estado

      -- helpers case-insensitive
      invTiene :: String -> Bool
      invTiene req = any (\(k,v) -> map toLower k == map toLower req && v > 0) (M.toList inv)

      flagTiene :: String -> Bool
      flagTiene req = any (\(k,v) -> map toLower k == map toLower req && v) (M.toList fl)

      tieneRecurso :: String -> Bool
      tieneRecurso req = not (null req) && (invTiene req || flagTiene req)

      apply (msgs, st) tr =
        case trapRequiereItem tr of
          Just reqItem ->
            if tieneRecurso reqItem
            then (msgs ++ ["Ves una trampa (" ++ trapId tr ++ ") pero tienes " ++ reqItem ++ " la evitas."], st)
            else
              let dmg = trapDaño tr
                  nuevaSalud = saludJugador st - dmg
                  st' = st { saludJugador = nuevaSalud }
                  msg = "Activaste trampa (" ++ trapId tr ++ "): " ++ trapDescripcion tr ++ " Recibes " ++ show dmg ++ " dano."
              in (msgs ++ [msg], st')
          Nothing ->
            let dmg = trapDaño tr
                nuevaSalud = saludJugador st - dmg
                st' = st { saludJugador = nuevaSalud }
                msg = "Trampa activa: " ++ trapDescripcion tr ++ ". Recibes " ++ show dmg ++ " dano."
            in (msgs ++ [msg], st')

      (mensajes, estadoFinal) = foldl apply ([], estado) traps
      texto = if null mensajes then "" else unlines mensajes
      -- Añadir línea de salud si hubo daño
      textoConSalud = if null texto
                      then ""
                      else texto ++ "\nSalud actual: " ++ show (saludJugador estadoFinal) ++ " / 100"

      -- comprobar muerte
      estadoConMuerte = if saludJugador estadoFinal <= 0
                        then estadoFinal { ejecutando = False }
                        else estadoFinal
      textoFinal = if saludJugador estadoFinal <= 0
                   then textoConSalud ++ "\nHas muerto por las heridas. Fin del juego."
                   else textoConSalud
  in (textoFinal, estadoConMuerte)


mostrarDir :: Direccion -> String
mostrarDir Norte = "Norte"
mostrarDir Sur   = "Sur"
mostrarDir Este  = "Este"
mostrarDir Oeste = "Oeste"
mostrarDir Centro = "Centro"

-- procesar comando (puro)
procesarComando :: Comando -> EstadoJuego -> (String, EstadoJuego)
procesarComando cmd estado = case cmd of

  CmdMirar ->
    let salaAct = lookupSala (ubicacion estado) (habitaciones estado)
        baseMsg = case salaAct of
          Nothing -> "Estas en un lugar desconocido."
          Just s  -> mostrarSala s (catalogoObjetos estado)
        estadoCondTxt = case estadoCondicion estado of
                          Nothing -> "Normal"
                          Just stt -> stt
        saludTxt = "Salud: " ++ show (saludJugador estado) ++ " / 100"
        full = unlines $ filter (not . null) [baseMsg, saludTxt, "Estado: " ++ estadoCondTxt]
    in (full, estado)

  CmdInventario ->
    let inv = inventarioJugador estado
        msg = if M.null inv then "Inventario vacio."
              else unlines $ ("Inventario:": [ itemLine oid qty | (oid,qty) <- M.toList inv ])
        itemLine oid qty = case M.lookup oid (catalogoObjetos estado) of
                             Just o -> "- " ++ objNombre o ++ " (" ++ oid ++ ") x" ++ show qty
                             Nothing -> "- " ++ oid ++ " x" ++ show qty
    in (msg, estado)

  CmdSalir ->
    ("Hasta luego. ¡Fin del juego!", estado { ejecutando = False })

  CmdIr dir ->
    let cur = ubicacion estado
        hs  = habitaciones estado
    in case lookupSala cur hs of
         Nothing -> ("No se dónde estas.", estado)
         Just sala ->
           case M.lookup dir (salidasSala sala) of
             Nothing -> ("No puedes ir en esa direccion.", estado)
             Just salida ->
               case bloqueadaPor salida of
                 Just llave -> ("La salida está bloqueada. Parece necesitar: " ++ llave, estado)
                 Nothing ->
                   let dest = destinoSalida salida
                   in if not (M.member dest hs)
                      then ("La salida apunta a una sala inexistente: " ++ dest, estado)
                      else
                        -- Si es la salida final, comprobamos requisitos aquí (no aplicamos trampas)
                        if dest == "Salida del Laberinto"
                        then
                          if puedeIntentarSalida estado
                          then
                            let estadoMov = estado { ubicacion = dest }
                                msg = "Te desplazas a " ++ dest ++ ". Has usado la llave y las monedas/mapa — ¡Has escapado del laberinto!"
                                estadoFinal = estadoMov { ejecutando = False }
                            in (msg, estadoFinal)
                          else
                            let faltaLlave = cantidadItem "llave_salida" (inventarioJugador estado) == 0
                                faltaMonedasOMapa = not (M.findWithDefault False "mapa_completo" (flags estado))
                                                   && not (tieneAlMenos "moneda_dorada" 8 (inventarioJugador estado))
                                msgFalta = unwords $ filter (not . null)
                                  [ if faltaLlave then "Te falta la llave que trae el oso." else ""
                                  , if faltaMonedasOMapa then "Te faltan las 8 monedas doradas." else ""
                                  ]
                            in ( "La puerta hacia fuera esta ahi, pero no puedes pasar: " ++ msgFalta
                               , estado )
                        else
                          -- movimiento normal: solo actualizamos ubicacion y devolvemos aviso.
                          let estadoMov = estado { ubicacion = dest }
                              aviso = "Te desplazas a " ++ dest ++ "."
                          in (aviso, estadoMov)
  
  CmdTomar nombreObj ->
    let key = map toLower (trim nombreObj)
        cur = ubicacion estado
        hs = habitaciones estado
        mSala = lookupSala cur hs
    in case mSala of
         Nothing -> ("Sala actual invalida.", estado)
         Just sala ->
           case M.lookup key (objetosSala sala) of
             Nothing -> ("No encuentro " ++ nombreObj ++ " aqui.", estado)
             Just qty ->
               if qty <= 0
               then ("No hay unidades de " ++ nombreObj ++ " aqui.", estado)
               else
                 let nuevaObjsSala = if qty == 1 then M.delete key (objetosSala sala)
                                     else M.insert key (qty - 1) (objetosSala sala)
                     nuevaSala = sala { objetosSala = nuevaObjsSala }
                     nuevasHabit = M.insert cur nuevaSala hs
                     -- añadir al inventario usando la clave en minusculas
                     nuevoInv = añadirInventario key 1 (inventarioJugador estado)
                     nuevoPeso = calcularPeso nuevoInv (catalogoObjetos estado)
                     estado' = estado { habitaciones = nuevasHabit
                                      , inventarioJugador = nuevoInv
                                      , pesoActual = nuevoPeso
                                      }
                     nombreLegible = case M.lookup key (catalogoObjetos estado) of
                                       Just o -> objNombre o
                                       Nothing -> key
                 in ("Has tomado: " ++ nombreLegible ++ ".", estado')

  CmdUsar nombreObj ->
    let inv = inventarioJugador estado
    in case M.lookup nombreObj inv of
         Nothing -> ("No tienes " ++ nombreObj ++ " en tu inventario.", estado)
         Just qty ->
           -- casos especiales: vendas, botiquin, llave_salidas, linterna
           case nombreObj of
             "vendas" ->
               let saludN = min 100 (saludJugador estado + 8)
                   inv' = quitarInventario "vendas" 1 inv
                   nuevoPeso = calcularPeso inv' (catalogoObjetos estado)
                   estado' = estado { saludJugador = saludN, inventarioJugador = inv', pesoActual = nuevoPeso }
               in ("Te vendaste. Salud actual: " ++ show saludN, estado')
             "botiquin" ->
               let saludN = min 100 (saludJugador estado + 15)
                   inv' = quitarInventario "botiquin" 1 inv
                   nuevoPeso = calcularPeso inv' (catalogoObjetos estado)
                   estado' = estado { saludJugador = saludN, inventarioJugador = inv', pesoActual = nuevoPeso }
               in ("Has usado el botiquín. Salud actual: " ++ show saludN, estado')
             "llave_salida" ->
               let salaCentralNombre = "Salida del Laberinto"  -- ajusta si tu mundo usa otro nombre
               in if ubicacion estado /= salaCentralNombre
                  then ("No puedes usar la llave de salida aqui. Debes usarla en: " ++ salaCentralNombre, estado)
                  else if not (tieneAlMenos "moneda_dorada" 8 (inventarioJugador estado))
                       then ("No puedes usar la llave de salida: te faltan 8 monedas doradas.", estado)
                       else
                         let inv' = quitarInventario "llave_salida" 1 (inventarioJugador estado)
                             estado' = estado { inventarioJugador = inv', ejecutando = False }
                         in ("Has abierto la puerta y escapado del laberinto.", estado')
             _ ->
              let -- helper: buscar descripción en el catálogo
                  descripcionSiExiste oid =
                    case M.lookup oid (catalogoObjetos estado) of
                      Just o -> objDesc o
                      Nothing -> ""

                  -- items que al 'usar' activan una bandera temporal/equipamiento
                  activadores = ["linterna","repelente","cuerda","machete","pistola_sedante","escopeta"]
                  activaEfecto oid st = st { flags = M.insert oid True (flags st) }

              in if descripcionSiExiste nombreObj /= "" && nombreObj `notElem` activadores
                  then -- si es una nota u objeto informativo, devolvemos su descripción
                    ( "Lees/Examinas " ++ nombreObj ++ ": " ++ descripcionSiExiste nombreObj
                    , estado )
                  else if nombreObj `elem` activadores
                    then -- activamos la bandera global para ese efecto
                      let st' = activaEfecto nombreObj estado
                      in ( "Has activado/encendido: " ++ nombreObj ++ ". Esto ayudara contra trampas que lo requieran.", st' )
                    else
                      -- intentar usar como llave en la sala (puede abrir puertas) o fallback
                      case tryUsarLlaveEnSala nombreObj estado of
                        Just estado' -> ("Usaste " ++ nombreObj ++ " para desbloquear algo.", estado')
                        Nothing -> ("Usas " ++ nombreObj ++ ". No sucede nada especial.", estado)

  CmdHablar npcName ->
    let cur = ubicacion estado
        hs = habitaciones estado
    in case lookupSala cur hs of
        Nothing -> ("No estoy en una sala valida.", estado)
        Just sala ->
          case lookupNPCCaseInsensitive npcName (npcsSala sala) of
            Nothing -> ("No hay nadie llamado " ++ npcName ++ " aqui.", estado)
            Just (claveReal, npc) ->
              let msgBase = npcDesc npc
                  nuevaConf = min 100 (npcConfianza npc + 10)
                  npc' = npc { npcConfianza = nuevaConf }
                  sala' = sala { npcsSala = M.insert claveReal npc' (npcsSala sala) }
                  hs' = M.insert cur sala' hs
                  estado' = estado { habitaciones = hs' }
                  clasif = clasificarAlianza nuevaConf
                  msg = "Confianza: " ++ show nuevaConf ++ ". " ++ clasif
              in (msg, estado')

-- función auxiliar: convierte un valor de confianza en clasificación
clasificarAlianza :: Int -> String
clasificarAlianza c
  | c >= 30  = "Parece aliado."
  | c <= -20 = "Parece hostil."
  | otherwise = "Parece neutral."

-- intentar usar llave en la sala actual: abre la primera salida que requiera exactamente esa llave
tryUsarLlaveEnSala :: NombreObjeto -> EstadoJuego -> Maybe EstadoJuego
tryUsarLlaveEnSala llave est =
  let cur = ubicacion est
      hs = habitaciones est
  in case lookupSala cur hs of
       Nothing -> Nothing
       Just sala ->
         let salMap = salidasSala sala
             matches = [ (d, s) | (d,s) <- M.toList salMap, bloqueadaPor s == Just llave ]
         in case matches of
              [] -> Nothing
              ((d,s):_) ->
                let s' = s { bloqueadaPor = Nothing }
                    salMap' = M.insert d s' salMap
                    sala' = sala { salidasSala = salMap' }
                    hs' = M.insert cur sala' hs
                in Just (est { habitaciones = hs' })
