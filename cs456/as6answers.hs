{-# LANGUAGE FlexibleInstances #-}
import Data.Monoid
import Data.Foldable
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

-- Problem 1
kar :: Show a => [a] -> MaybeT IO a
kar [] = mzero
kar (x:xs) = do
		liftIO . print $ x
		return x

kdr :: Show a => [a] -> MaybeT IO a
kdr [] = mzero
kdr (x:xs) = do 
	liftIO . print $ xs
	return xs


grandkidscats :: Maybe Bool
grandkidscats = noElem Cat <$> (runListT. (kids >=> kids >=> pets)) Bill


--fold with monoid will use monioid as combinator.


instance Monoid (Parser String) where 
	mempty = mzero
	mappend = mplus

word = ask >>= foldMap string


tree p = fork p 'mplus' leaf p
wrap p = do{char '('; xt <- tree p; char ')'; return xt}
leaf p = do{symb "Leaf"; x<-p; return. Leaf. read $ x}
fork p = do{symb "Fork"; xt <- wrap p; yt <- wrap p; return $ Fork xt yt}

apply p dict = runStateT (runReaderT p dict)

btree dict cs = x where Just(x,y) = apply (tree word) (map show dict) cs







