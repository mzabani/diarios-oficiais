{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Buscador where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Char as C
import Control.Applicative
import Brdocs
import qualified Data.Attoparsec.Text as AT

newtype DocumentoDiario = DocumentoDiario [(Int, DocumentoToken)] deriving Show
data DocumentoToken = TokenTexto T.Text | TokenCpf (T.Text, Cpf) | TokenCnpj (T.Text, Cnpj) | TokenNumeroGeral T.Text deriving (Show)

-- | Um trecho de texto que representa algarismos alternando com barras (/), pontos (.), hífens (-) e vírgulas (,) apenas.
newtype NumeroGeral = NumeroGeral T.Text deriving Show

data ParsedToken = ParsedText T.Text | ParsedCpf (T.Text, Cpf) | ParsedCnpj (T.Text, Cnpj) | ParsedNumeroGeral NumeroGeral deriving Show
parseConteudoDiario :: T.Text -> DocumentoDiario
parseConteudoDiario "" = DocumentoDiario []
parseConteudoDiario txt = case documentoComChars of
                              Left err     -> error $ "Impossível.. qualquer texto pode ser parseado. Erro: " ++ err
                              Right l -> DocumentoDiario $ List.reverse $ fst $ List.foldl' (\(res, pos) (len, novoToken) -> ((pos, novoToken) : res, pos + len)) ([], 0) $ fmap convertAndLength l
  where documentoComChars :: Either String [Either NumeroGeral T.Text]
        documentoComChars = AT.parseOnly (AT.many1 simpleTokenParser) txt
        -- Plano 1: Pegar cada Char pesa BASTANTE, mas nos dá garantia de usar o melhor Parser possível a cada caracter.. depois aglutinamos caracteres em sequência em Tokens de Text
        -- Plano 2: Criar Parser que pega trechos de texto sem números (Cnpjs e CPFs sempre começam com números..) pra reduzir alocações
        -- Por enquanto estamos com plano 2, já que o 1 ficou lento demais da conta
        simpleTokenParser :: AT.Parser (Either NumeroGeral T.Text)
        simpleTokenParser = fmap Left numGeralParser <|> fmap Right (AT.takeWhile1 (not . C.isDigit))
        numGeralParser :: AT.Parser NumeroGeral
        numGeralParser = NumeroGeral <$> AT.takeWhile1 (\c -> C.isDigit c || c == '-' || c == '/' || c == ',' || c == '.' || c == '\n')
        -- TODO: Detectar valores em R$ com Brdocs.valorParser também (isso sim será interessante)
        convertAndLength :: Either NumeroGeral T.Text -> (Int, DocumentoToken)
        convertAndLength (Right t) = (T.length t, TokenTexto t)
        convertAndLength (Left (NumeroGeral numComoTexto)) =
          let token = case (parseCpf numComoTexto, parseCnpj numComoTexto) of
                        (Just cpf, _) -> TokenCpf (numComoTexto, cpf)
                        (_, Just cnpj) -> TokenCnpj (numComoTexto, cnpj)
                        _ -> TokenNumeroGeral numComoTexto
          in (T.length numComoTexto, token)


-- | Essa função ainda não procura nomes com erros de escrita; encontra apenas matches exatos.
buscarNomeApenasMatches :: T.Text -> DocumentoDiario -> [(Int, T.Text)]
buscarNomeApenasMatches "" _ = []
buscarNomeApenasMatches nome (DocumentoDiario tokens) = concatMap (\case (pos, TokenTexto t) -> buscarNome pos t
                                                                         _                   -> []) tokens
  where buscarNome pos texto = let matches = T.breakOnAll (T.toLower nome) (T.toLower texto)
                                   nomeLength = T.length nome
                               in reverse $ fst $ List.foldl' (\(resultados, _) (tudoAntes, aposComNome) -> ((T.length tudoAntes + pos, T.take nomeLength aposComNome) : resultados, T.length tudoAntes + nomeLength)) ([], 0) matches