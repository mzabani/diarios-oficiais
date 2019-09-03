module Brdocs
    ( Cpf
    , Cnpj
    , printCpf
    , printCnpj
    , printCpfSoNumeros
    , printCnpjSoNumeros
    , cpfValido
    , cnpjValido
    , valorParser
    , parseCpf
    , parseCnpj
    , cpfParser
    , cnpjParser
    ) where

import qualified Data.Text as S
import qualified Data.Attoparsec.Text as AT
import qualified Data.Ratio as Ratio
import Data.Ratio (Ratio)
import Import

newtype Cpf = Cpf S.Text deriving (Eq, Ord, Show)
newtype Cnpj = Cnpj S.Text deriving (Eq, Ord, Show)

printCpf :: Cpf -> S.Text
printCpf (Cpf t) = t

printCpfSoNumeros :: Cpf -> S.Text
printCpfSoNumeros = S.replace "." "" . S.replace "-" "" . printCpf

printCnpj :: Cnpj -> S.Text
printCnpj (Cnpj t) = t

printCnpjSoNumeros :: Cnpj -> S.Text
printCnpjSoNumeros = S.replace "/" "" . S.replace "." "" . S.replace "-" "" . printCnpj

cpfValido :: S.Text -> Bool
cpfValido = isJust . parseCpf

cnpjValido :: S.Text -> Bool
cnpjValido = isJust . parseCnpj

-- ^ Recebe as partes do Cpf separadas por pontos e hífen e retorna True se o CPF for válido.
verificarCpfInterno :: S.Text -> S.Text -> S.Text -> S.Text -> Bool
verificarCpfInterno p1 p2 p3 c = maybe False id $ do
  (np1, ns1) <- prod [10, 9, 8] [11, 10, 9] p1 3
  (np2, ns2) <- prod [7, 6, 5] [8, 7, 6] p2 3
  (np3, ns3) <- prod [4, 3, 2] [5, 4, 3] p3 3
  case digitosConfirmadores c of
    Just (c1,c2) -> do
      let resto1 = 10 * (np1 + np2 + np3) `mod` 11
      let d1 = if resto1 == 10 then 0 else resto1
      let resto2 = 10 * (ns1 + ns2 + ns3 + 2 * d1) `mod` 11
      let d2 = if resto2 == 10 then 0 else resto2
      return $ d1 == c1 && d2 == c2
    _ -> return False

-- ^ Recebe as partes do Cnpj separadas por pontos, barra e hífen e retorna True se o CNPJ for válido.
verificarCnpjInterno :: S.Text -> S.Text -> S.Text -> S.Text -> S.Text -> Bool
verificarCnpjInterno p1 p2 p3 p4 c = maybe False id $ do
      (np1, ns1) <- prod [5, 4] [6, 5] p1 2
      (np2, ns2) <- prod [3, 2, 9] [4, 3, 2] p2 3
      (np3, ns3) <- prod [8, 7, 6] [9, 8, 7] p3 3
      (np4, ns4) <- prod [5, 4, 3, 2] [6, 5, 4, 3] p4 4
      case digitosConfirmadores c of
        Just (c1,c2) -> do
          let resto1 = (np1 + np2 + np3 + np4) `mod` 11
          let d1 = if resto1 <= 2 then 0 else 11 - resto1
          let resto2 = (ns1 + ns2 + ns3 + ns4 + 2 * d1) `mod` 11
          let d2 = if resto2 <= 2 then 0 else 11 - resto2
          return $ d1 == c1 && d2 == c2
        _ -> return False

-- TODO: Strict params!
prod :: [Int] -> [Int] -> S.Text -> Int -> Maybe (Int, Int)
prod p1s p2s t compEsp = prod' p1s p2s (nums t) compEsp 0 (0, 0)
  where
    prod' (p1:p1s) (p2:p2s) (n:ns) compEsp compReal (res1, res2) = prod' p1s p2s ns compEsp (compReal + 1) (res1 + p1*n, res2 + p2*n)
    prod' [] [] [] compEsp compReal res = if compEsp == compReal then Just res else Nothing
    prod' _ _ _ _ _ _ = Nothing

nums :: S.Text -> [Int]
nums t = catMaybes $ S.foldr (\x acc -> digitToIntMay x : acc) [] t

digitosConfirmadores :: S.Text -> Maybe (Int, Int)
digitosConfirmadores t
  | S.length t /= 2 = Nothing
  | otherwise       = case nums t of
                        (d1:d2:[]) -> Just (d1, d2)
                        _          -> Nothing

parseMaybe :: AT.Parser a -> S.Text -> Maybe a
parseMaybe p s = case AT.parseOnly (p <* AT.endOfInput) s of
                   Left  _ -> Nothing
                   Right r -> Just r

-- ^ Parser de CPF que aceita apenas números
cpfParserApenasNumeros :: AT.Parser Cpf
cpfParserApenasNumeros = do
  p1 <- AT.take 3
  p2 <- AT.take 3
  p3 <- AT.take 3
  c  <- AT.take 2
--  AT.endOfInput
  if verificarCpfInterno p1 p2 p3 c == False then fail "Cpf inválido" else return . Cpf $ S.concat [p1, ".", p2, ".", p3, "-", c]

-- ^ Parser de CPF que aceita apenas o formato bonitinho (###.###.###-##)
cpfParserBonitinho :: AT.Parser Cpf
cpfParserBonitinho = do
  p1 <- AT.take 3
  AT.char '.'
  p2 <- AT.take 3
  AT.char '.'
  p3 <- AT.take 3
  AT.char '-'
  c <- AT.take 2
--  AT.endOfInput
  if verificarCpfInterno p1 p2 p3 c == False then fail "Cpf inválido" else return . Cpf $ S.concat [p1, ".", p2, ".", p3, "-", c]

cpfParser :: AT.Parser Cpf
cpfParser = cpfParserBonitinho <|> cpfParserApenasNumeros

-- ^ Parser de CNPJ que aceita apenas o formato numérico
cnpjParserApenasNumeros :: AT.Parser Cnpj
cnpjParserApenasNumeros = do
  p1 <- AT.take 2
  p2 <- AT.take 3
  p3 <- AT.take 3
  p4 <- AT.take 4
  c <- AT.take 2
  if verificarCnpjInterno p1 p2 p3 p4 c == False then fail "Cnpj inválido" else return . Cnpj $ S.concat [p1, ".", p2, ".", p3, "/", p4, "-", c]

-- ^ Parser de CNPJ que aceita apenas o formato bonitinho (##.###.###/####-##)
cnpjParserBonitinho :: AT.Parser Cnpj
cnpjParserBonitinho = do
  p1 <- AT.take 2
  AT.char '.'
  p2 <- AT.take 3
  AT.char '.'
  p3 <- AT.take 3
  AT.char '/'
  p4 <- AT.take 4
  AT.char '-'
  c <- AT.take 2
  if verificarCnpjInterno p1 p2 p3 p4 c == False then fail "Cnpj inválido" else return . Cnpj $ S.concat [p1, ".", p2, ".", p3, "/", p4, "-", c]

-- ^ Parser que aceita "R$ número", "R$número", "número reais" ou "número BRL" com número tendo separadores de milhar ou decimal opcionais
-- TODO: Implementar!
valorParser :: AT.Parser (Ratio Int)
valorParser = undefined

cnpjParser :: AT.Parser Cnpj
cnpjParser = cnpjParserBonitinho <|> cnpjParserApenasNumeros

parseCpf :: S.Text -> Maybe Cpf
parseCpf = parseMaybe cpfParser

parseCnpj :: S.Text -> Maybe Cnpj
parseCnpj = parseMaybe cnpjParser