-- Arthur Napolitano, Kevin Omidvaran
-- CS456
-- 11-16-17
-- In class exercise 6
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad
import Data.Maybe
import Control.Applicative
import Data.Char

-- Question 1
car :: [a] -> Maybe a
car (x:xs) = Just x
car [] = Nothing

cdr :: [a] -> Maybe [a]
cdr (x:xs) = Just xs 
cdr [] = Nothing


cadddr ::  [a] -> Maybe a
cadddr = cdr >=> cdr >=> cdr >=> car

kar :: Show a => [a] -> MaybeT IO a

kar (x:xs) =  MaybeT $ do 
                        putStrLn (show x)
                        (return $ Just x)

kar [] = MaybeT $ do 
             (return Nothing)      

kdr :: Show a => [a] -> MaybeT IO [a]

kdr (x:xs) = MaybeT $ do 
             putStrLn (show xs)
             (return $ Just xs)
kdr [] = MaybeT $ do 
             (return Nothing)

kadddr xs = kdr xs >>= kdr >>= kdr >>= kar




-- Question 2

data Person = Bill | Bob | Amy | Val | Jim | Kim deriving (Show, Eq)

ks = [(Bill, [Bob, Amy]), (Bob, [Val, Jim]), (Amy, []), (Val, [Kim])]


kids :: Person -> ListT Maybe Person
kids = ListT . flip lookup ks


data Pet = Cat | Dog deriving (Show, Eq)
ps = [(Bill, [Dog]), (Bob, [Cat, Dog]), (Val, []), (Kim, [Cat]), (Jim, [])]


pets :: Person -> ListT Maybe Pet
pets = ListT . flip lookup ps

grandkids person =   do
                            maylistkid <- kids person
                            se <- kids (maylistkid)
                            return se

petOfGrandKids person = do
                        grandkid <-  (grandkids person)
                        pet <- pets grandkid
                        return pet


grandkidscats person = case runListT (petOfGrandKids person) of
                        Nothing -> Nothing

                        Just xs -> Just (not (elem Cat xs))


--Question 3
type Parser a = StateT String Maybe a
--item :: Parser Char
--item = StateT (\cs -> case cs of
--                    "" -> Nothing
--                    (c:cs) -> (Just(c,cs)))

--sat :: (Char -> Bool) -> Parser Char
--sat p = do {c <- item; if p c then return c else mzero}

--char :: Char -> Parser Char
--char c = sat (c ==)

--string :: String -> Parser String
--string "" = return ""
--string (c:cs) = do {char c; string cs; return (c:cs)}


-- Question 4
type Dictionary = [String]
type Parser' a = ReaderT Dictionary (StateT String Maybe) a

item :: Parser' Char
item = ReaderT (\d -> (StateT(\cs -> case cs of
                                        "" -> Nothing
                                        (c:cs) -> (Just(c,cs)))))

sat :: (Char -> Bool) -> Parser' Char
sat p = do {c <- item; if p c then return c else mzero}


char :: Char -> Parser' Char
char c = sat (c ==)

string :: String -> Parser' String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}



word  = do {d <- ask; (comp d) }

comp (d:ds) = do {string d `mplus` (comp ds)}
comp [] = mzero

data Btree a = Leaf a | Fork (Btree a)(Btree a) deriving Show

data Beatle = John | Paul | George | Ringo deriving (Show, Eq, Enum, Bounded, Read)

beatles = map show [(John)..(Ringo)]


symb cs = token (string cs)

symbHelper p' cs = runReaderT (string cs)

token p = do {a <- p; space; return a}

space = Main.many (sat isSpace)

many p = many1 p +++ return []

many1 p = do {a <- p; as <- Main.many p; return (a:as)}

expr =  (forkop <*> subtree <*> subtree) `mplus` term
term = leafop <*> (arg word)

arg :: Parser' String -> Parser' Beatle
arg p = ReaderT (\d -> StateT (\cs ->  maybeRead (runStateT (runReaderT p d ) cs )))


subtree = do {symb "("; n <- expr; symb ")"; return n}


p +++ q = ReaderT(\d ->  StateT(\cs -> runStateT (runReaderT (p `mplus` q) d) cs))


maybeRead Nothing = Nothing
maybeRead (Just (s,t)) = case read s :: Beatle of
                            a -> Just (a,t)


leafop = do {symb "Leaf"; return Leaf} 
forkop = do {symb "Fork"; return Fork}

btree dictionary s = btreeHelp (runStateT ((runReaderT expr dictionary)) s)


test1 = "Fork (Leaf John) (Fork (Fork (Leaf Paul) (Leaf George))(Leaf Ringo))"

test2 = "Fork (Leaf John) (Fork (Fork (Leaf Paul) (Leaf George))(Leaf Ringo))"

test3 = "Fork (Leaf John) (Fork (Fork (Leaf Paul) (Leaf None))(Leaf Ringo))"


btreeHelp (Just (a,b)) = a
btreeHelp Nothing  = (Leaf John)

