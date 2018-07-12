module Lib
    ( someFunc
    ) where

import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Data.Conduit (Consumer, Sink, ($$))
import Data.XML.Types (Event)
import Text.XML.Stream.Parse

data Page = Page String
    deriving Show

someFunc :: IO ()
someFunc = putStrLn "someFunc"

parsePage :: MonadThrow m => Consumer Event m (Maybe Page)
parsePage = Page $ tag' "page" $ tag' "title"

parsePages :: MonadThrow m => Sink Event m (Maybe [Page])
parsePages = tagNoAttr "mediawiki" $ many parsePage

readFile :: String -> IO ()
readFile path = do
    pages <- runResourceT $
        parseFile def path $$ force "file not found" parsePages
    print pages
