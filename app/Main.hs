-- Autor: Kevin Briceño
-- Carnet: 15-11661
-- app/Main.hs

module Main where

import System.IO( hFlush, stdout, stdin, stderr, hSetEncoding, utf8)
import System.Info (os)

import Engine.Persistence (cargarMundo)
import Engine.Parser (parsearComando)
import Engine.Core (procesarComando, resolverTrampasAlEntrar)
import Engine.Types

import qualified Data.Map as M
import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

-- Punto de entrada del programa
main :: IO ()
main = do
  -- Forzar encoding UTF-8 para evitar problemas con acentos/ñ en Windows
  hSetEncoding stderr utf8
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8

  putStrLn "Cargando mundo desde 'mundo.txt'..."
  res <- cargarMundo "mundo.txt"
  case res of
    Left err -> putStrLn ("Error cargando mundo: " ++ err)
    Right (mapSalas, mapItems) -> do
      -- Extrae todos los NPCs actualmente definidos dentro de las salas
      let npcPairs = collectNPCs mapSalas
      -- Vaciamos los NPCs de las salas y se reasignan aleatoriamente
      let salasSinNPCs = M.map (\s -> s { npcsSala = M.empty }) mapSalas
      -- Elegir sala inicial sólo entre las que comienzan con "Habitacion "
      let allRoomNames = M.keys salasSinNPCs
          candidateRooms = filter ("Habitacion " `isPrefixOf`) allRoomNames
      chosenStart <- if null candidateRooms
                    then do
                      -- fallback: cualquiera si no hay habitaciones etiquetadas
                      i <- randomRIO (0, length allRoomNames - 1)
                      return (allRoomNames !! i)
                    else do
                      j <- randomRIO (0, length candidateRooms - 1)
                      return (candidateRooms !! j)
      -- Preparamos la lista de salas para asignar NPCs: todas menos la sala elegida
      let restoSalas = filter (/= chosenStart) allRoomNames
      -- mezclamos las salas restantes para distribuir NPCs (opcional)
      restoShuffled <- shuffle restoSalas
      -- asignamos NPCs usando restoShuffled
      let numNPCs = length npcPairs
          roomsForNPCs = take numNPCs (cycle restoShuffled)
          salasConNPCs = assignNPCs salasSinNPCs npcPairs roomsForNPCs
      -- asignamos la sala inicial
      let salaInicial = chosenStart
          estadoInicial = EstadoJuego
            { habitaciones      = salasConNPCs
            , catalogoObjetos   = mapItems
            , ubicacion         = salaInicial
            , inventarioJugador = M.empty
            , ejecutando        = True
            , capacidad         = 50
            , pesoActual        = 0
            , saludJugador      = 100
            , estadoCondicion   = Nothing
            , flags             = M.empty
            }
      -- Aplicar trampas (si las hubiera) en la sala inicial antes de comenzar
      let salaObj = fromMaybe (error "Sala inicial no encontrada") (M.lookup salaInicial (habitaciones estadoInicial))
          (msgInicio, estadoConTrampas) = resolverTrampasAlEntrar salaObj estadoInicial

      putStrLn ("Mundo cargado. Sala inicial asignada aleatoriamente: " ++ salaInicial)
      when (not (null msgInicio)) $ putStrLn msgInicio

      -- Si la trampa mató al jugador, informar y no entrar al gameLoop
      if not (ejecutando estadoConTrampas)
        then putStrLn "Has muerto antes de empezar. Fin del juego."
        else do
          putStrLn "Escribe comandos (mirar, inventario, ir <direccion>, tomar <objeto>, usar <objeto>, hablar <npc>, salir)."
          gameLoop estadoConTrampas

-- Colecciona todos los NPCs que estén en cualquier sala: [(id,npc)]
collectNPCs :: ContenedorSalas -> [(NombreNPC, NPC)]
collectNPCs salas =
  concatMap (\s -> M.toList (npcsSala s)) (M.elems salas)

-- Devuelve el nuevo map de salas con NPCs colocados.
assignNPCs :: ContenedorSalas -> [(NombreNPC, NPC)] -> [NombreSala] -> ContenedorSalas
assignNPCs salas npcPairs rooms =
  let paired = zip npcPairs rooms
      insertOne acc ((nid,npc), rname) =
        case M.lookup rname acc of
          Nothing -> acc
          Just sala -> M.insert rname (sala { npcsSala = M.insert nid npc (npcsSala sala) }) acc
  in foldl insertOne salas paired

-- Extrae aleatoriamente un elemento hasta vaciar la lista.
shuffle :: [a] -> IO [a]
shuffle xs = go xs []
  where
    go [] acc = return (reverse acc)
    go ys acc = do
      i <- randomRIO (0, length ys - 1)
      let (before, z:after) = splitAt i ys
          ys' = before ++ after
      go ys' (z:acc)

-- Elegir sala inicial
elegirSalaInicial :: ContenedorSalas -> NombreSala
elegirSalaInicial salas
  | M.member "Habitacion Centro" salas  = "Habitacion Centro"
  | M.member "Sala de Estar" salas      = "Sala de Estar"
  | otherwise = case M.keys salas of
                  (x:_) -> x
                  []    -> error "No hay salas definidas en el mundo."

-- Bucle principal. Llama al motor puro procesarComando.
gameLoop :: EstadoJuego -> IO ()
gameLoop estado =
  if not (ejecutando estado)
    then putStrLn "Juego finalizado."
    else do
      putStr "> "
      hFlush stdout
      linea <- getLine
      case parsearComando linea of
        Nothing -> putStrLn "Comando inválido." >> gameLoop estado
        Just comando -> do
          let (msg, estado') = procesarComando comando estado
          putStrLn msg
          putStrLn "Escribe comandos (mirar, inventario, ir <direccion>, tomar <objeto>, usar <objeto>, hablar <npc>, salir)."
          gameLoop estado'
