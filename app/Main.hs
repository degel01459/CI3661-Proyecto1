-- Autor: Kevin Briceño
-- Carnet: 15-11661
-- app/Main.hs
module Main where

import Engine.Persistence (cargarMundo)
import Engine.Parser (parsearComando)
import Engine.Core (procesarComando)
import Engine.Types

import qualified Data.Map as M
import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)

-- Punto de entrada
main :: IO ()
main = do
  putStrLn "Cargando mundo desde 'mundo.txt'..."
  res <- cargarMundo "mundo.txt"
  case res of
    Left err -> putStrLn ("Error cargando mundo: " ++ err)
    Right (mapSalas, mapItems) -> do
      -- extrae todos los NPCs actualmente definidos dentro de las salas
      let npcPairs = collectNPCs mapSalas            -- [(id,npc)]
      -- vaciamos los NPCs de las salas (los re-asignaremos aleatoriamente)
      let salasSinNPCs = M.map (\s -> s { npcsSala = M.empty }) mapSalas
      -- shuffle de salas (IO)
      salasShuffled <- shuffle (M.keys salasSinNPCs)
      -- elegimos sala del jugador y rooms para NPCs
      let (jugadorSala, restoSalas) = case salasShuffled of
            (x:xs) -> (x, xs)
            []     -> error "No hay salas definidas en el mundo."
      -- asignamos NPCs a salas aleatorias
      let numNPCs = length npcPairs
          roomsForNPCs = take numNPCs (cycle restoSalas) -- si hay menos salas que NPCs, repetimos
          salasConNPCs = assignNPCs salasSinNPCs npcPairs roomsForNPCs

      let salaInicial = jugadorSala
          estadoInicial = EstadoJuego
            { habitaciones      = salasConNPCs
            , catalogoObjetos   = mapItems
            , ubicacion         = salaInicial
            , inventarioJugador = M.empty
            , ejecutando        = True
            , capacidad         = 50            -- valor inicial de ejemplo
            , pesoActual        = 0
            , saludJugador      = 100
            , estadoCondicion   = Nothing
            , flags             = M.empty
            }
      putStrLn ("Mundo cargado. Sala inicial asignada aleatoriamente: " ++ salaInicial)
      putStrLn "Escribe comandos (mirar, inventario, ir <direccion>, tomar <objeto>, usar <objeto>, hablar <npc>, salir)."
      gameLoop estadoInicial

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

-- Shuffle sencillo: extrae aleatoriamente un elemento hasta vaciar la lista.
shuffle :: [a] -> IO [a]
shuffle xs = go xs []
  where
    go [] acc = return (reverse acc)
    go ys acc = do
      i <- randomRIO (0, length ys - 1)
      let (before, z:after) = splitAt i ys
          ys' = before ++ after
      go ys' (z:acc)

-- Elegir sala inicial (mantengo la preferencia por 'Habitacion Centro (Salida)' si existe)
elegirSalaInicial :: ContenedorSalas -> NombreSala
elegirSalaInicial salas
  | M.member "Habitacion Centro (Salida)" salas = "Habitacion Centro (Salida)"
  | M.member "Sala de Estar" salas              = "Sala de Estar"
  | otherwise = case M.keys salas of
                  (x:_) -> x
                  []    -> error "No hay salas definidas en el mundo."

-- Bucle principal (I/O). Llama al motor puro procesarComando.
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
