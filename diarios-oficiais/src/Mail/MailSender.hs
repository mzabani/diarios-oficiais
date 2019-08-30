{-# LANGUAGE OverloadedStrings #-}
module Mail.MailSender where

import Aws
import Aws.Ses.Core
import Aws.Ses.Commands.SendRawEmail
import Network.Mail.Mime
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.ByteString.Lazy

data Email = Email {
    to :: T.Text,
    subject :: T.Text,
    body :: T.Text,
    from :: T.Text
}

sesConfiguration :: SesConfiguration NormalQuery
sesConfiguration = sesHttpsPost sesUsWest2

-- TODO: Don't use simpleAws!
sendMail :: Configuration -> Email -> IO SendRawEmailResponse
sendMail conf e = do
    mail <- simpleMail (Address { addressName = Nothing, addressEmail = to e }) (Address { addressName = Nothing, addressEmail = from e }) (subject e) "" (L.fromStrict (body e)) []
    rawMsg <- renderMail' mail
    simpleAws conf sesConfiguration SendRawEmail {
        srmDestinations = [to e]
        , srmRawMessage = RawMessage (toStrict rawMsg)
        , srmSource = Just $ Sender (from e)
    }