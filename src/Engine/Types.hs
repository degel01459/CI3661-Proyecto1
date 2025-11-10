module Engine.Types
  ( ID
  , Nombre
  , NombreSala
  , NombreObjeto
  , NombreNPC
  , Direccion(..)
  , Salida(..)
  , ItemTipo(..)
  , Objeto(..)
  , Inventario
  , Cantidad
  , Monstruo(..)
  , Disposicion(..)
  , NPC(..)
  , Sala(..)
  , EstadoJuego(..)
  , Comando(..)
  , ContenedorSalas
  , ContenedorObjetos
  ) where

import qualified Data.Map as M

-- Alias simples
type ID = String
type Nombre = String
type NombreSala = String
type NombreObjeto = String
type NombreNPC = String
type Cantidad = Int

-- Direcciones posibles
data Direccion = Norte | Sur | Este | Oeste
  deriving (Eq, Ord, Show, Read)

-- Salida: destino + posible bloqueo (requiere un item) + descripción opcional
data Salida = Salida
  { destinoSalida      :: NombreSala
  , bloqueadaPor       :: Maybe NombreObjeto
  , descripcionSalida  :: Maybe String
  } deriving (Eq, Show)

-- Tipos de objetos
data ItemTipo = Llave | Arma | Consumible | Herramienta | Miscelaneo
  deriving (Eq, Show)

-- Definición de objeto del catálogo
data Objeto = Objeto
  { objId     :: NombreObjeto   -- identificador
  , objNombre :: Nombre         -- nombre legible
  , objDesc   :: String
  , objTipo   :: ItemTipo
  , objPeso   :: Int
  } deriving (Eq, Show)

-- Inventario (apilable): mapa objeto -> cantidad
type Inventario = M.Map NombreObjeto Cantidad

-- Monstruo simple
data Monstruo = Monstruo
  { monId   :: ID
  , monNombre :: Nombre
  , monHp   :: Int
  , monAtk  :: Int
  , monLoot :: [(NombreObjeto, Cantidad)]
  } deriving (Eq, Show)

-- Disposición de NPCs
data Disposicion = Amistoso | Neutral | Agresivo | Ambiguo
  deriving (Eq, Show)

-- NPC
data NPC = NPC
  { npcId       :: NombreNPC
  , npcNombre   :: Nombre
  , npcDesc     :: String
  , npcDisp     :: Disposicion
  , npcConfianza :: Int        -- escala -100..100
  , npcInventario :: Inventario
  , npcHp       :: Int
  , npcAtk      :: Int
  , npcLoot     :: [(NombreObjeto, Cantidad)]
  } deriving (Eq, Show)

-- Sala: salidas (mapa), inventario en sala, monstruos y npcs
data Sala = Sala
  { salaId        :: NombreSala
  , salaNombre    :: Nombre
  , salaDesc      :: String
  , salidasSala   :: M.Map Direccion Salida
  , objetosSala   :: Inventario
  , monstruosSala :: M.Map ID Monstruo
  , npcsSala      :: M.Map NombreNPC NPC
  } deriving (Eq, Show)

-- Estado completo del juego
data EstadoJuego = EstadoJuego
  { habitaciones   :: M.Map NombreSala Sala
  , catalogoObjetos :: M.Map NombreObjeto Objeto
  , ubicacion      :: NombreSala
  , inventarioJugador :: Inventario
  , ejecutando     :: Bool
  , capacidad      :: Int
  , pesoActual     :: Int
  , saludJugador   :: Int
  , estadoCondicion :: Maybe String     -- "paralizado" o normal
  , flags          :: M.Map String Bool -- banderas globales ("mapa_completo")
  } deriving (Eq, Show)

-- Comandos que el jugador puede ejecutar (versión inicial)
data Comando
  = CmdIr Direccion
  | CmdMirar
  | CmdTomar NombreObjeto
  | CmdInventario
  | CmdUsar NombreObjeto
  | CmdHablar NombreNPC
  | CmdHurtar NombreNPC
  | CmdAtacar (Either NombreNPC ID)      -- atacar a NPC por nombre o monstruo por ID
  | CmdVigilar NombreNPC
  | CmdSalir
  deriving (Eq, Show)

type ContenedorSalas = M.Map NombreSala Sala
type ContenedorObjetos = M.Map NombreObjeto Objeto