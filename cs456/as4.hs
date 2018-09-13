-- Arthur Napolitano, Kevin Omidvaran
-- CS456
-- 10-26-17
-- In class exercise 4

import Control.Monad
import Data.Maybe
import Control.Applicative
import Data.Char

newtype Parser a = Parser {parse :: String -> Maybe (String, a)} 




instance Monad Parser where 
    return x = Parser (\cs -> Just(cs,x) ) 
    m >>= k  = Parser $ \s -> parse m s >>= (\(s',a) -> parse (k a) s')


instance Applicative Parser where 
    pure x = Parser (\cs -> Just(cs,x) ) 
    (<*>) = ap

instance Alternative Parser where
     (<|>) = mplus
     empty = mzero 

instance Functor Parser where
    fmap = liftM


instance MonadPlus Parser where 
    mzero = Parser (\s -> Nothing)
    x `mplus` y =  Parser (\s -> if (isNothing (parse x s)) then parse y s else parse x s)



item = Parser (\cs -> case cs of
                    "" -> Nothing
                    (c:cs) -> (Just(cs,c)))


sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}


(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> parse (p `mplus` q) cs)



many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- Main.many p; return (a:as)}



space :: Parser String
space = Main.many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> Maybe(String,a)
apply p = parse (do {space; p})

data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving Show



a = "Fork (Leaf 1) (Fork (Fork (Leaf 2) (Leaf 3)) (Leaf 4))"

b = "Leaf 2"

c = "(Leaf 1) Fork  (Leaf 2)"

--expr2 :: Parser (Btree Int)
leafop :: Parser (Int -> Btree Int)
forkop :: Parser (Btree Int -> Btree Int -> Btree Int)
expr =  (forkop <*> subtree <*> subtree) `mplus` term
term = leafop <*> digit
subtree = do {symb "("; n <- expr; symb ")"; return n}
digit = do {x <- token (sat isDigit); return (ord x - ord '0')}

leafop = do {symb "Leaf"; return Leaf} 
forkop = do {symb "Fork"; return Fork}


btree s = btreeHelp (parse expr s)


-- If the parser returns a nothing we return a Leaf 0
btreeHelp (Just (a,b)) = b
btreeHelp Nothing  = (Leaf 0) 

x = parse (token (string "Leaf1")) "Leaf1"

y = parse digit "1"
