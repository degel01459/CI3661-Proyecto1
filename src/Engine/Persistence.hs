-- Autor: Kevin Briceño
-- Carnet: 15-11661
-- src/Engine/Persistence.hs
module Engine.Persistence (cargarMundo) where
import Engine.Types
import qualified Data.Map as M
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (foldM)

-- trim
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

lower :: String -> String
lower = map toLower

-- split en bloques por '---'
splitBloques :: String -> [String]
splitBloques s = filter (not . null) $ map unlines $ splitWhen (== "---") (map trim $ lines s)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = case dropWhile p xs of
  [] -> []
  xs' -> let (chunk, rest) = break p xs' in chunk : splitWhen p rest

-- helpers case-insensitive
startsCI :: String -> String -> Bool
startsCI pref s = lower pref `isPrefixOf` lower s

-- extrae valor despues de un prefijo (case-insensitive)
extraeCampo :: String -> [String] -> Maybe String
extraeCampo pref ls = case [ drop (length pref) line | line <- ls, startsCI pref line ] of
  (v:_) -> Just (trim v)
  []    -> Nothing

-- parsea una SALIDA: "Norte -> Destino (requiere=llave_id)"
parseSalida :: String -> Maybe (Direccion, Salida)
parseSalida raw =
  let s = trim raw
      -- buscar "->" como substring
      findArrow :: String -> Maybe (String, String)
      findArrow str = findAt 0 str
        where
          findAt _ [] = Nothing
          findAt i (a:b:rest)
            | a == '-' && b == '>' = Just (take i str, drop (i+2) str)
            | otherwise = findAt (i+1) (b:rest)
          findAt _ _ = Nothing

      (leftPart, afterArrow) = case findArrow s of
                                Just (l, r) -> (trim l, trim r)
                                Nothing     -> (s, "") -- no arrow
      -- extraer posible "(requiere=...)" si aparece al final del afterArrow
      (destTxt, requer) = case break (== '(') afterArrow of
        (before, '(' : restPar) ->
          let inside = takeWhile (/= ')') restPar
              req = case wordsWhen (== '=') inside of
                      (_:val:_) -> Just (trim val)
                      _ -> Nothing
          in (trim before, req)
        _ -> (trim afterArrow, Nothing)
  in case lower leftPart of
       "norte" -> Just (Norte, Salida destTxt requer Nothing)
       "n"     -> Just (Norte, Salida destTxt requer Nothing)
       "sur"   -> Just (Sur, Salida destTxt requer Nothing)
       "s"     -> Just (Sur, Salida destTxt requer Nothing)
       "este"  -> Just (Este, Salida destTxt requer Nothing)
       "e"     -> Just (Este, Salida destTxt requer Nothing)
       "oeste" -> Just (Oeste, Salida destTxt requer Nothing)
       "o"     -> Just (Oeste, Salida destTxt requer Nothing)
       _ -> Nothing

-- helpers locales
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s'' where (w,s'') = break p s'

-- parsea "OBJETO: id" o "OBJETO: id xN" -> (id, cantidad)
parseObjetoEnSala :: String -> (NombreObjeto, Cantidad)
parseObjetoEnSala raw =
  let s = trim raw
      tokens = words s
  in case tokens of
       [] -> ("", 1)
       [single] -> (single, 1)
       _ ->
         let lastTok = last tokens
             nameTokens = init tokens
             name = unwords nameTokens
         in if (not (null lastTok) && head lastTok == 'x' && all (`elem` ['0'..'9']) (tail lastTok))
               then (name, read (tail lastTok))
               else if all (`elem` ['0'..'9']) lastTok
                  then (name, read lastTok)
                  else (s, 1)  -- fallback: todo el string es el id, cantidad 1

-- parse de MONSTRUO: acepta "MONSTRUO: id" o con DESC/HP/ATK (simple)
parseMonstruo :: [String] -> Maybe Monstruo
parseMonstruo ls =
  case extraeCampo "MONSTRUO:" ls of
    Just body ->
      let idToken = takeWhile (not . isSpace) body
          display = trim body
          (hp, atk) = defaultStats idToken
      in Just (Monstruo idToken display hp atk [])
    Nothing -> Nothing

-- default stats según id/name (ajústalas)
defaultStats :: String -> (Int, Int)
defaultStats s = case lower s of
  _ | "oso" `isIn` s -> (30,8)
  _ | "leon" `isIn` s -> (20,6)
  _ | "lobo" `isIn` s -> (14,5)
  _ | "serpiente" `isIn` s -> (6,2)
  _ | "araña" `isIn` s -> (4,1)
  _ -> (10,2)
  where isIn pat txt = pat `isPrefixOf` lower txt || pat `elem` words (lower txt)

-- parsing helper para TRAMPA dentro de un bloque SALA
parseTrampas :: [String] -> M.Map String Trampa
parseTrampas ls =
  let bloquesTrap = splitWhen (\l -> startsCI "TRAMPA:" l) ls
      parseOne trapLines =
        case extraeCampo "TRAMPA:" trapLines of
          Nothing -> Nothing
          Just tid ->
            let desc = fromMaybe "" (extraeCampo "DESC:" trapLines)
                req  = extraeCampo "REQUIERE:" trapLines
                dan  = case extraeCampo "DANIO:" trapLines of
                         Just n -> case reads n of ((v,_):_) -> v; _ -> 0
                         Nothing -> 0
                tipoTxt = fmap lower (extraeCampo "TIPO:" trapLines)
                ttype = case tipoTxt of
                           Just t | "ven" `isPrefixOf` t -> TrampaVenenosa
                           Just t | "aguj" `isPrefixOf` t -> TrampaAgujero
                           Just t | "cand" `isPrefixOf` t -> TrampaCandado
                           _ -> TrampaNone
                tr = Trampa tid ttype desc req dan True
            in Just (tid, tr)
  in M.fromList (catMaybes (map parseOne (groupTrapBlocks ls)))

-- agrupa lineas en secciones de trap (simple)
groupTrapBlocks :: [String] -> [[String]]
groupTrapBlocks ls = filter (not . null) $ splitWhen (startsCI "TRAMPA:") ls
-- parse NPC block
parseNPC :: [String] -> Maybe NPC
parseNPC ls =
  case extraeCampo "NPC:" ls of
    Just idToken ->
      let desc = fromMaybe "" (extraeCampo "DESC:" ls)
          name = idToken
      in Just (NPC name name desc Neutral 0 M.empty 12 2 [])
    Nothing -> Nothing

-- parse ITEM block
parseItem :: [String] -> Maybe Objeto
parseItem ls =
  case extraeCampo "ITEM:" ls of
    Just idToken ->
      let desc = fromMaybe "" (extraeCampo "DESC:" ls)
          tipoTxt = fmap (map toLower) (extraeCampo "TIPO:" ls)
          tipo = case tipoTxt of
                   Just t | "llave" `isPrefixOf` t -> Llave
                   Just t | "arma" `isPrefixOf` t -> Arma
                   Just t | "consum" `isPrefixOf` t -> Consumible
                   Just t | "herr" `isPrefixOf` t -> Herramienta
                   _ -> Miscelaneo
          peso = case extraeCampo "PESO:" ls of
                   Just p -> case reads p of
                               ((n,_):_) -> n
                               _ -> 1
                   Nothing -> 1
      in Just (Objeto idToken idToken desc tipo peso)
    Nothing -> Nothing

-- parse SALA block
parseSala :: [String] -> Maybe Sala
parseSala ls =
  case extraeCampo "SALA:" ls of
    Just nombreSala ->
      let desc = fromMaybe "" (extraeCampo "DESC:" ls)
          salidasRaw = [ drop (length "SALIDA:") line | line <- ls, startsCI "SALIDA:" line ]
          salidasParsed = catMaybes (map parseSalida salidasRaw)
          salidasMap = M.fromList salidasParsed
          objetosRawLines = [ drop (length "OBJETO:") line | line <- ls, startsCI "OBJETO:" line ]
          objetosPairs = map (parseObjetoEnSala . trim) objetosRawLines
          objetosMap = foldr (\(k,c) acc -> if null k then acc else M.insertWith (+) k c acc) M.empty objetosPairs
          trampasParsed = parseTrampas ls
          npcsParsed = case parseNPC ls of
            Just n -> M.singleton (npcId n) n
            Nothing -> M.empty
      in Just (Sala nombreSala nombreSala desc salidasMap objetosMap trampasParsed npcsParsed)
    Nothing -> Nothing

-- parsea un bloque genérico
parseBloque :: String -> Either String (Either Objeto Sala)
parseBloque blk =
  let ls = filter (not . null) $ map trim (lines blk)
  in case () of
       _ | startsCI "ITEM:" (unwords ls) ->
             case parseItem ls of
               Just o -> Right (Left o)
               Nothing -> Left ("Error parseando ITEM en bloque:\n" ++ blk)
         | startsCI "SALA:" (unwords ls) ->
             case parseSala ls of
               Just s -> Right (Right s)
               Nothing -> Left ("Error parseando SALA en bloque:\n" ++ blk)
         | startsCI "MONSTRUO:" (unwords ls) ->
             case parseMonstruo ls of
               Just m -> Left $ "MONSTRUO blocks are currently not stored as ITEM; define MONSTRUO separately: " ++ show (monId m)
               Nothing -> Left ("Error parseando MONSTRUO en bloque:\n" ++ blk)
         | startsCI "NPC:" (unwords ls) ->
             Left ("NPC blocks must be handled via SALA entries or ITEMs in this simple parser.")
         | otherwise -> Left ("Bloque no reconocido (esperaba ITEM: o SALA:): " ++ take 80 blk)

-- carga mundo
cargarMundo :: FilePath -> IO (Either String (ContenedorSalas, ContenedorObjetos))
cargarMundo fp = do
  contenido <- readFile fp
  let bloques = splitBloques contenido
      parsed = map parseBloque bloques
      errs = [e | Left e <- parsed, "MONSTRUO blocks" `notElem` words e] -- keep errors
  if not (null errs)
    then return $ Left (unlines errs)
    else do
      let rights' = [r | Right r <- parsed]
          items = [i | Left i <- rights']   -- here Left contains items in our design
          salas = [r | Right r <- rights']
          mapItems = M.fromList [(objId o, o) | o <- items]
          mapSalas = M.fromList [(salaId s, s) | s <- salas]
      -- validaciones sencillas
      let objetosNoEncontrados = [ (objName, sName)
                                 | s <- salas
                                 , let sName = salaId s
                                 , (objName, _) <- M.toList (objetosSala s)
                                 , not (M.member objName mapItems)
                                 ]
          salidasInvalidas = [ (dest, salaId s)
                             | s <- salas
                             , (_, Salida dest _ _) <- M.toList (salidasSala s)
                             , not (M.member dest mapSalas)
                             ]
      if not (null objetosNoEncontrados)
        then return $ Left ("Objetos referenciados no encontrados en catálogo: " ++ show objetosNoEncontrados)
        else if not (null salidasInvalidas)
          then return $ Left ("Salidas apuntan a salas inexistentes: " ++ show salidasInvalidas)
          else return $ Right (mapSalas, mapItems)
