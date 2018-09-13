import Control.Monad             (guard)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
--import Control.Monad.Error

import Data.Char

readBoth :: IO (Maybe String)
readBoth = runMaybeT $ do 
                        a <- funA2
                        funB2 a
          
funA2 :: MaybeT IO String
funA2 = MaybeT funA

funB2 :: String -> MaybeT IO String
funB2 = MaybeT . funB

funA :: IO (Maybe String)
funA = do 
          putStrLn "What is your name?"
          name <- getLine
          guard $ not (null name)
          return $ Just name

funB :: String -> IO (Maybe String)
funB name = do 
            putStrLn ("hello, " ++ name)
            putStrLn "how old are you?"
            age <- getLine
            guard (all isDigit age)
            return $ Just age



main = do 
        readBoth