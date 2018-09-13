-- Arthur Napolitano, Kevin Omidvaran
-- CS456
-- 11-7-17
-- In class exercise 5

import Parselib
import Data.Char

data Sexpr = Symbol String | Number Int | Nil | Cons Sexpr Sexpr

data BASIC = Input String | Let (String,BASIC) | B Double | Print String


instance Show BASIC where
  show (Input a) = "Input" ++ a
  show (Let(a,b)) = "let = " ++ a ++ show b 
  show (B a) = "B" ++ show a
  show (Print a) = "Print " ++ show a
instance Show Sexpr where 
    show (Symbol x) =  "symbol " ++ x 
    show (Number x) = "Num" ++ show x 
    show Nil = "()"
    show (Cons x y) = "cons" ++ "(" ++ show x ++ showCdr y ++ ")"

--instance Show Sexpr where 
--    show (Symbol x) = x 
--    show (Number x) =  show x 
--    show Nil = "()"
--    show (Cons x y) =  "(" ++ show x ++ showCdr y ++ ")"

showCdr :: Sexpr -> String
showCdr Nil = ""
showCdr (Cons x Nil) = "cons" ++ " " ++ show x ++ (show Nil)
showCdr (Cons x v@(Cons y z)) = "cons" ++ " " ++ show x ++ showCdr v
showCdr (Cons x y) = "cons" ++ " " ++ show x ++ " . " ++ show y
showCdr x = " . " ++ show x


--showCdr :: Sexpr -> String
--showCdr Nil = ""
--showCdr (Cons x Nil) = " " ++ show x
--showCdr (Cons x v@(Cons y z)) = " " ++ show x ++ showCdr v
--showCdr (Cons x y) = " " ++ show x ++ " . " ++ show y
--showCdr x = " . " ++ show x



--simpleParser = do{ symb "print"; return(Print)}  +++ do{symb "input"; return (Input)} +++ do{symb "let"; return (Let)}

car (Cons a b) = a
car _  = Nil
 
cdr (Cons a b) = b
cdr _ = Nil

cadddr = car . cdr. cdr . cdr

caadddr = car.car . cdr. cdr . cdr

getLineNum (Cons a b) = case a of
              (Number c) -> c
              _ -> 0





s = do {symb "("; symb ")"; return (Nil)}  +++ do {symb "("; n <- token e; symb ")"; return (n)} +++ do {n <- a; return (n)}
    +++ do {symb "("; n<- token s; symb "."; n2 <- token s; symb ")"; return (Cons n n2)} 


e = do {symb "("; n <- token e; symb ")"; n2 <- token e; return (Cons n n2) }  +++ do {n <- token s; n2 <- token e; return (Cons n n2)}
    +++ do {n <- s; return (Cons n Nil)}

  

a = do{n <- symbol; return (Symbol n)} +++ do{ n <- number; return (n)}

symbol = do {n <- (first); n2 <- token (many symbolic); return (n:n2)}



symbolic = do {n <- digit; return (intToDigit n)} +++ do {n <- first; return (n)}


number = do{n <- many1 myDigit; return (Number (read n :: Int))}


myDigit = sat isDigit


-- now parses quotes, need to be able to parse floats like 4.0!
misc = do {symb "<"; return ('<')} +++
       do {symb ">"; return ('>')} +++
       do {symb "^"; return ('^')} +++
       do {symb "+"; return ('+')} +++
       do {symb "-"; return ('-')} +++
       do {symb "*"; return ('*')} +++
       do {symb "/"; return ('/')} +++
       do {symb "="; return ('=')} +++
       do {string "\""; return ('\"')} +++
       do {string "\'"; return ('\'')}

first = misc +++ letter

p st =  fst (head (parse s st))


s3 = Cons (Number 1) (Cons (Number 2) (Cons (Number 3) Nil))
s1 = "(1 2 3)"

s2 = "(define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))"


-- FOR BASIC: Assume no floats, then the scheme parser can parse these string values
f = "(define foo \'((100 input \"what is the value of a\" a ) (110 input \"what is the value of b\" b ) (120 input \"what is the value of b\" c ) (130 let d = ((b * b) - (4 * (a * c)))) (140 print d) (150 end)))"

test = "(define foo \'((100 input \"what is the value of a\" a ) (110 input \"what is the value of b\" b ) (120 input \"what is the value of b\" c ) (130 let d = ((b * b) - (4 * (a * c))))) )"

test2 = "(130 let d = ((b*b) - (4.0 * (a * c))) )" 

ab = caadddr $ p f 

b = (cdr (cdr (cdr (cdr (cdr ab)))))

c = cdr(cdr (b))

main = do
  putStrLn "What is the value of A"
  a <- getLine
  putStrLn "What is the value of B"
  b <- getLine
  putStrLn "What is the value of C"
  c <- getLine
  let a' = read a :: Int
  let b' = read b :: Int
  let c' = read c :: Int
  let d = ((b' * b') - (4 * (a' * c')))
  putStrLn $ show d
