module Engine.Parser (parsearComando) where

import Engine.Types
import Data.Char (isSpace, toLower)
import Data.List (stripPrefix)

-- Trim de espacios en ambos lados
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Minúsculas
aMinusculas :: String -> String
aMinusculas = map toLower

-- Parsea una palabra a Direccion (acepta abreviaturas)
parsearDireccion :: String -> Maybe Direccion
parsearDireccion s = case aMinusculas (trim s) of
  "norte" -> Just Norte
  "n"     -> Just Norte
  "sur"   -> Just Sur
  "s"     -> Just Sur
  "este"  -> Just Este
  "e"     -> Just Este
  "oeste" -> Just Oeste
  "o"     -> Just Oeste
  _       -> Nothing

-- Parsea la entrada del usuario (String) a Comando
-- Acepta: "mirar", "inventario", "salir", "ir norte", "ir a norte", "tomar <objeto>", "usar <objeto>"
parsearComando :: String -> Maybe Comando
parsearComando raw =
  let s = trim raw
      w = words (aMinusculas s)
  in case w of
    [] -> Nothing
    ["mirar"] -> Just CmdMirar
    ["salir"] -> Just CmdSalir
    ["inventario"] -> Just CmdInventario
    ["inv"] -> Just CmdInventario
    ("ir":dir:_) -> CmdIr <$> parsearDireccion dir
    ("ir":[]) -> Nothing
    ("tomar":xs) -> Just (CmdTomar (trim (unwords xs)))
    ("agarrar":xs) -> Just (CmdTomar (trim (unwords xs)))
    ("usar":xs) -> Just (CmdUsar (trim (unwords xs)))
    ("hablar":xs) -> Just (CmdHablar (trim (unwords xs)))
    _-> Nothing
