module AwsUtils where

import qualified RIO.ByteString as B
import Aws
import qualified Data.Attoparsec.ByteString.Char8 as Parsec

-- | Retorna a configuração Amazon do usuário IAM com as permissões necessárias pra esta aplicação funcionar.
-- TODO: Ainda não usamos a Amazon.. usar o S3 para armazenar os diários em PDF seria ótimo
createAwsConfiguration :: IO Configuration
createAwsConfiguration = do
    chavesCsv <- B.readFile "./keys/aws-IAM-diarios-fetcher-accessKeys.csv"
    let [accessKeyId, secretKey] = either (error "Erro ao parsear o que deveria ser um arquivo de uma linha com chaves AWS separadas por vírgula") id $
                                        Parsec.parseOnly (Parsec.sepBy Parsec.takeByteString (Parsec.char ',') <* Parsec.endOfInput) chavesCsv
    creds <- makeCredentials accessKeyId secretKey
    return Configuration {
        timeInfo = Timestamp
        , credentials = creds
        , logger = defaultLog Error
        , proxy = Nothing
    }