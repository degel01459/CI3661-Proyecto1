-- Autor: Kevin Briceño
-- Carnet: 15-11661
-- src/Engine/Core.hs
module Engine.Core (procesarComando) where

import Engine.Types
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- helpers de inventario
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

-- buscar sala
lookupSala :: NombreSala -> ContenedorSalas -> Maybe Sala
lookupSala = M.lookup

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

-- resolver trampas: si el jugador tiene el item requerido evita daño,
resolverTrampasAlEntrar :: Sala -> EstadoJuego -> (String, EstadoJuego)
resolverTrampasAlEntrar sala estado =
  let traps = M.elems (trampasSala sala)
      inv = inventarioJugador estado
      apply (msgs, st) tr =
        case trapRequiereItem tr of
          Just reqItem ->
            if M.member reqItem inv
            then (msgs ++ ["Ves una trampa (" ++ trapId tr ++ ") pero tienes " ++ reqItem ++ " — la evitas."], st)
            else
              let dmg = trapDaño tr
                  nuevaSalud = saludJugador st - dmg
                  st' = st { saludJugador = nuevaSalud }
                  msg = "Activaste trampa (" ++ trapId tr ++ "): " ++ trapDescripcion tr ++ " — recibes " ++ show dmg ++ " daño."
              in (msgs ++ [msg], st')
          Nothing ->
            let dmg = trapDaño tr
                nuevaSalud = saludJugador st - dmg
                st' = st { saludJugador = nuevaSalud }
                msg = "Trampa activa: " ++ trapDescripcion tr ++ ". Recibes " ++ show dmg ++ " daño."
            in (msgs ++ [msg], st')
      (mensajes, estadoFinal) = foldl apply ([], estado) traps
      texto = if null mensajes then "" else unlines mensajes
  in (texto, estadoFinal)

mostrarDir :: Direccion -> String
mostrarDir Norte = "Norte"
mostrarDir Sur   = "Sur"
mostrarDir Este  = "Este"
mostrarDir Oeste = "Oeste"

-- procesar comando (puro)
procesarComando :: Comando -> EstadoJuego -> (String, EstadoJuego)
procesarComando cmd estado = case cmd of

  CmdMirar ->
    let salaAct = lookupSala (ubicacion estado) (habitaciones estado)
        msg = case salaAct of
          Nothing -> "Estás en un lugar desconocido."
          Just s  -> mostrarSala s (catalogoObjetos estado)
    in (msg, estado)

  CmdInventario ->
    let inv = inventarioJugador estado
        msg = if M.null inv then "Inventario vacío."
              else unlines $ ("Inventario:": [ itemLine oid qty | (oid,qty) <- M.toList inv ])
        itemLine oid qty = case M.lookup oid (catalogoObjetos estado) of
                             Just o -> "- " ++ objNombre o ++ " (" ++ oid ++ ") x" ++ show qty
                             Nothing -> "- " ++ oid ++ " x" ++ show qty
    in (msg, estado)

  CmdSalir ->
    ("Hasta luego. ¡Fin del juego!", estado { ejecutando = False })

  CmdIr dir ->
    let cur = ubicacion estado
        hs = habitaciones estado
    in case lookupSala cur hs of
         Nothing -> ("No sé dónde estás.", estado)
         Just sala ->
           case M.lookup dir (salidasSala sala) of
             Nothing -> ("No puedes ir en esa dirección.", estado)
             Just salida ->
               case bloqueadaPor salida of
                 Just llave -> ("La salida está bloqueada. Parece necesitar: " ++ llave, estado)
                 Nothing ->
                   let dest = destinoSalida salida
                   in if M.member dest hs
                      then
                        let estadoMov = estado { ubicacion = dest }
                            salaDest = fromMaybe (error "sala inexistente") (lookupSala dest hs)
                            (msgTrampas, estadoConTrampas) = resolverTrampasAlEntrar salaDest estadoMov
                            aviso = "Te desplazas a " ++ dest ++ "."
                            fullMsg = if null msgTrampas then aviso else aviso ++ "\n" ++ msgTrampas
                        in (fullMsg, estadoConTrampas)
                      else ("La salida apunta a una sala inexistente: " ++ dest, estado)

  CmdTomar nombreObj ->
    let cur = ubicacion estado
        hs = habitaciones estado
        mSala = lookupSala cur hs
    in case mSala of
         Nothing -> ("Sala actual inválida.", estado)
         Just sala ->
           case M.lookup nombreObj (objetosSala sala) of
             Nothing -> ("No encuentro " ++ nombreObj ++ " aquí.", estado)
             Just qty ->
               if qty <= 0
               then ("No hay unidades de " ++ nombreObj ++ " aquí.", estado)
               else
                 let nuevaObjsSala = if qty == 1 then M.delete nombreObj (objetosSala sala)
                                     else M.insert nombreObj (qty - 1) (objetosSala sala)
                     nuevaSala = sala { objetosSala = nuevaObjsSala }
                     nuevasHabit = M.insert cur nuevaSala hs
                     nuevoInv = añadirInventario nombreObj 1 (inventarioJugador estado)
                     nuevoPeso = calcularPeso nuevoInv (catalogoObjetos estado)
                     estado' = estado { habitaciones = nuevasHabit
                                      , inventarioJugador = nuevoInv
                                      , pesoActual = nuevoPeso
                                      }
                 in ("Has tomado: " ++ nombreObj ++ ".", estado')

  CmdUsar nombreObj ->
    let inv = inventarioJugador estado
    in case M.lookup nombreObj inv of
         Nothing -> ("No tienes " ++ nombreObj ++ " en tu inventario.", estado)
         Just qty ->
           -- casos especiales: vendas, antidoto, llaves, linterna
           case nombreObj of
             "vendas" ->
               let saludN = min 100 (saludJugador estado + 8)
                   inv' = quitarInventario "vendas" 1 inv
                   nuevoPeso = calcularPeso inv' (catalogoObjetos estado)
                   estado' = estado { saludJugador = saludN, inventarioJugador = inv', pesoActual = nuevoPeso }
               in ("Te vendaste. Salud actual: " ++ show saludN, estado')
             "antidoto" ->
               let inv' = quitarInventario "antidoto" 1 inv
                   nuevoPeso = calcularPeso inv' (catalogoObjetos estado)
                   estado' = estado { estadoCondicion = Nothing, inventarioJugador = inv', pesoActual = nuevoPeso }
               in ("Has aplicado el antídoto. Ya no estás envenenado.", estado')
             -- llaves: intentan desbloquear salidas en la sala actual
             _ ->
               case tryUsarLlaveEnSala nombreObj estado of
                 Just estado' -> ("Usaste " ++ nombreObj ++ " para desbloquear algo.", estado')
                 Nothing -> ("Usas " ++ nombreObj ++ ". No sucede nada especial.", estado)

  CmdHablar npcName ->
    let cur = ubicacion estado
        hs = habitaciones estado
    in case lookupSala cur hs of
         Nothing -> ("No estoy en una sala válida.", estado)
         Just sala ->
           case M.lookup npcName (npcsSala sala) of
             Nothing -> ("No hay nadie llamado " ++ npcName ++ " aquí.", estado)
             Just npc ->
               let msgBase = npcDesc npc
                   -- aumentar confianza ligeramente al hablar
                   nuevaConf = min 100 (npcConfianza npc + 10)  -- +10 por hablar (ajustable)
                   npc' = npc { npcConfianza = nuevaConf }
                   sala' = sala { npcsSala = M.insert npcName npc' (npcsSala sala) }
                   hs' = M.insert cur sala' hs
                   estado' = estado { habitaciones = hs' }
                   clasif = clasificarAlianza nuevaConf
                   msg = msgBase ++ " " ++ "Confianza: " ++ show nuevaConf ++ ". " ++ clasif
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
