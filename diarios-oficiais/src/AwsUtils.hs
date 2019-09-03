module AwsUtils where

import Aws

-- | Retorna a configuração Amazon do usuário IAM "diarios-fetcher", que possui as permissões necessárias pra esta aplicação funcionar.
createAwsConfiguration :: IO Configuration
createAwsConfiguration = do
    -- TODO: No futuro não deixar chaves no código, mas carregá-las do disco (outros devs não terão acesso às chaves desta forma!)
    let accessKeyId = "AKIAJIKUJ5C4VQ3SMB7Q"
    let secretKey = "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"
    creds <- makeCredentials accessKeyId secretKey
    return Configuration {
        timeInfo = Timestamp
        , credentials = creds
        , logger = defaultLog Error
        , proxy = Nothing
    }