module Main where

import Network.Http.Client
import Data.Maybe
import Network.URI
import System.Environment
import Text.HTML.TagSoup


getResponseBody :: String -> IO ByteString
getResponseBody x = get x concatHandler


openURL :: String -> IO String
openURL x = (getResponseBody x).unpack headers 

parseString :: IO ()
parseString = do
        [args, searchValue] <- getArgs 
        tags <- parseTags <$> openURL args
        let test = map f $ sections(~== searchValue) tags
        putStr (unlines test) 
       where
        f :: [Tag String] -> String
        f = dequote . unwords . words . fromTagText . head . filter isTagText

        dequote ('\"':xs) | last xs == '\"' = init xs
        dequote x = x




main :: IO ()
main = parseString
