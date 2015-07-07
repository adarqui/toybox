{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TB.Attoparsec.Parsers.Services (
) where

import Control.Monad
import           Control.Applicative
--import           Data.Attoparsec.Lazy
import           Data.Attoparsec.Text
--import           Data.ByteString.Lazy.Char8 (ByteString)
--import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           GHC.Generics
import Data.Maybe
import Data.Either

data Protocol
  = ProtoTCP
  | ProtoUDP
  | ProtoBoth
  | ProtoUnknown
  deriving (Eq, Show, Generic)

data Service = Service {
  name  :: Text,
  port  :: Int,
  proto :: Protocol,
  desc  :: Text
} deriving (Eq, Show, Generic)

textToProto :: Text -> Protocol
textToProto proto
  | proto == "udp" = ProtoUDP
  | proto == "tcp" = ProtoTCP
  | otherwise = ProtoUnknown

readServices :: IO Text
readServices = T.readFile "/etc/services"

whiteSpace :: Parser ()
whiteSpace = skipMany (char ' ' <|> char '\t')

-- | Parse the service name field
--
-- >>> parseOnly parseServiceName "ssh"
-- Right "ssh"
--
parseServiceName :: Parser Text
parseServiceName = do
  service <- many (letter <|> char '-')
  return $ T.pack service
-- | Parse the port field
--
-- >>> parseOnly parsePort 22
-- Right 22
--
parsePort :: Parser Int
parsePort = do
  port <- many digit
  return (read port :: Int)

-- | Parse the protocol field
--
-- >>> parseOnly parseProtocol "udp"
-- Right ProtoUDP
--
-- >>> parseOnly parseProtocol "tcp"
-- Right ProtoTCP
--
-- >>> parseOnly parseProtocol "unk"
-- Right ProtoUnknown
--
parseProtocol :: Parser Protocol
parseProtocol = do
  proto <- (string "tcp" <|> string "udp")
  return $ textToProto proto

-- | Parse a /etc/services record
--
-- >>> parseOnly parseService "msg-icp  28/udp  # some description"
-- Right (Service {name = "msg-icp", port = 28, proto = UDP, desc = "some description"})
--
parseService :: Parser Service
parseService = do
  service <- parseServiceName
  whiteSpace
  port <- parsePort
  void $ char '/'
  proto <- parseProtocol
  whiteSpace
  void $ string "# "
  desc <- takeText
  return $ Service service port proto desc

-- | Parse all of /etc/services into Service records
parseServices :: IO [Service]
parseServices = do
--  liftM (rights . map (parseOnly parseService) . T.lines) readServices
    rights . map (parseOnly parseService) . T.lines <$> readServices
