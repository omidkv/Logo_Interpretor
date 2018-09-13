-- Arthur Napolitano, Kevin Omidvaran
-- CS456
-- 11-7-17
-- In class exercise 5
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
import Parselib
import Data.Char
import Prelude hiding (reverse)
import Data.IORef
import Data.List hiding (reverse)
import Data.Monoid
import Graphics.Rendering.OpenGL hiding (get,scale)
import Graphics.UI.GLUT hiding (get,scale)
import Control.Monad hiding (join)
import Control.Monad.State hiding (join)
import Control.Monad.Writer hiding (join)
import Control.Monad.Trans hiding (join)
import Control.Monad.State hiding (join)

type Point = (GLfloat, GLfloat)

instance Num Point where (x, y) + (x', y') = (x + x', y + y')

type Colour = (GLfloat, GLfloat, GLfloat)
data Plumber = Plumber Point GLfloat
data Box = Box GLfloat GLfloat GLfloat GLfloat deriving Show

red   = (1, 0, 0)
green = (0, 1, 0)
blue  = (0, 0, 1)

data Graphic = Straight GLfloat | Invisible GLfloat | Bend GLfloat | Join Graphic Graphic | Fork Graphic Graphic | Err String | Stop | Paint Colour Graphic deriving Show

join = foldr1 Join
fork = foldr1 Fork

renderStraight :: Point -> Point -> StateT Plumber IO ()
renderStraight (x0, y0) (x1, y1) =
    lift $ renderPrimitive Lines $ mapM_ vertex [Vertex2 x0 y0, Vertex2 x1 y1]

degToRad d = (d / 360) * 2 * pi

move l t' = modify (\(Plumber (x, y) t) -> Plumber (x + l * cos t, y + l * sin t) (t + t'))

render :: Graphic -> StateT Plumber IO ()
render (Stop) = move 0 0
render (Err _) = move 0 0

render (Straight length) = do
  Plumber p _ <- get
  move length 0
  Plumber q _ <- get
  renderStraight p q

render (Invisible length) = move length 0

render (Bend angle) = move 0 (degToRad angle)

render (Join g g') = do
  render g
  render g'

render (Fork g g') = do
  p <- get
  render g
  put p
  render g'
  put p

render (Paint (r', g', b') g) = do
  lift $ currentColor $= Color4 r' g' b' 1
  render g

instance Monoid Box where
    mempty = Box 0 0 0 0
    Box xMin yMin xMax yMax `mappend` Box x0 y0 x1 y1 =
        Box (min x0 xMin) (min y0 yMin) (max x1 xMax) (max y1 yMax)

forward :: GLfloat -> StateT Plumber (Writer Box) ()
forward length = do
  move length 0
  Plumber (x, y) _ <- get
  tell $ Box x y x y

boundingBox :: Graphic -> StateT Plumber (Writer Box) ()
boundingBox (Stop) = forward 0
boundingBox (Err _) = forward 0
boundingBox (Straight  length) = forward length
boundingBox (Invisible length) = forward length

-- change move to 10 to see example 3
boundingBox (Bend angle) = move (-1) (degToRad angle)

boundingBox (Join g g') = do
  boundingBox g
  boundingBox g'

boundingBox (Fork g g') = do
  p <- get
  boundingBox g
  put p
  boundingBox g'
  put p

boundingBox (Paint (r', g', b') g) = boundingBox g

mirror (Bend angle)         = Bend (-angle)
mirror (Join g g')          = mirror g `Join` mirror g'
mirror (Fork g g')          = mirror g `Fork` mirror g'
mirror (Paint color g)      = Paint color (mirror g)
mirror g                    = g

reverse (Join g g')        = reverse g' `Join` reverse g
reverse (Fork g g')        = reverse g  `Fork` reverse g'
reverse (Paint color g)    = Paint color (reverse g)
reverse g                  = g

scale s (Straight  length) = Straight  $ s*length
scale s (Invisible length) = Invisible $ s*length
scale s (Join g g')        = scale s g `Join` scale s g'
scale s (Fork g g')        = scale s g `Fork` scale s g'
scale s (Paint color g)    = Paint color (scale s g)
scale s g                  = g

-- Compute bounding box and draw centered figure
draw g = do
  let Box xMin yMin xMax yMax = execWriter (execStateT (boundingBox g) (Plumber (0, 0) 0))
  let (dx, dy) = (xMax - xMin, yMax - yMin)
  let s = 2 / max dx dy
  let (x, y) = (-s * (xMax + xMin) / 2, -s * (yMax + yMin) / 2)
  runStateT (render (scale s g)) (Plumber (x, y) 0)

repeat' n g = join $ genericTake n (repeat g)

polygon n = repeat' n . Join (Straight 1) . Bend $ 360 / (fromInteger n)

-- Koch curve

koch angle 0 = Straight 1
koch angle n = scale 2 . kochStep angle $ koch angle (n-1)

kochStep angle g = join [g, Bend (-angle), g, Bend (2*angle), g, Bend (-angle), g]

-- Gosper curve

gosper 0 = Straight 1
gosper n = gosperStep $ gosper (n-1)

gosperStep g = join [Bend 15, g, Bend (-60), g', Bend (-120), g', Bend 60, g, Bend 120, g, g, Bend 60, g', Bend (-75)]
  where g' = mirror $ reverse g

-- Sierpinski tree

sierpinski 0 = Straight 1
sierpinski n = scale 0.5 . sierpinskiStep $ sierpinski (n-1)

sierpinskiStep g = Straight 2 `Join` fork [Bend (-120) `Join` g, Bend 0 `Join` g, Bend 120 `Join` g]

-- Monkey tree

d = (sqrt 3) / 3

monkey 0 = Straight 1
monkey n = monkeyStep $ monkey (n-1)

monkeyStep g = join [Bend (-60), mirror g, reverse g, Bend 60, g, Bend 60, reverse g, Bend 150, scale d . reverse $ g, scale d . mirror $ g, Bend (-60), scale d . mirror . reverse $ g, Bend (-60), scale d . mirror . reverse $ g, scale d g, Bend (-90), mirror . reverse $ g, g]

-- Logo

starfish angle step = join . concat $ take 90 [[Straight 1, Bend angle] | angle <- [angle, angle + step .. ]]

stars angle = repeat' 5 $ join . concat $ take 8 [[Straight i, Bend angle] | i <- [1..]]

logo n x dx y dy = join . concat $ take n [[Straight (x + i*dx), Bend (y + i*dy)] | i <- [1..]]

starfish' angle step = logo 90 1 0 angle step

stars' angle = repeat' 5 $ logo 8 1 1 angle 0

-- Marroquin pattern

row n = join [repeat' n $ polygon 4 `Join` Invisible 20, Bend 180, Invisible $ fromIntegral n * 20, Bend (-90), Invisible 20, Bend (-90)]

grid n = join [Bend (-135), Invisible $ sqrt 2 * 10 * fromIntegral (n-1), Bend 135, repeat' n $ row n]

marroquin n = fork [Bend 120 `Join` g, Bend 60 `Join` g, g] where g = grid n

-- Wow

wow = scale 1.5 $ repeat' 71 $ (repeat' 360 $ Straight 1 `Join` Bend 1) `Join` Bend 5

--interaction

bindings :: KeyboardMouseCallback
bindings key keystate modifiers positions =
    putStrLn (show (key, keystate, modifiers, positions))

motion :: MotionCallback
motion position = putStrLn (show position)

increment n = if n == 5 then 1 else n + 1
--increment n = if n == 36 then 1 else n + 1

quickTest :: State Bool Graphic
quickTest = do { return (Straight 10);}

f a = return (Join a (Bend 45))

g a = return (Join a (Straight 30))


main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Haskell Plumbing Graphics"

  counter <- newIORef 1

  let timer = addTimerCallback 1000 $ do {modifyIORef counter increment; postRedisplay Nothing; timer}

  --displayCallback $= display counter
  displayCallback $= myDisplay

  addTimerCallback 1000 timer

  mainLoop
  
display counter = do
  clear [ColorBuffer]

  n <- readIORef counter
--  draw $ polygon n
--  draw $ Paint red $ monkey n
  draw $ Paint green $ repeat' 3 $ Join (koch 60 n) (Bend 120)
--  draw $ Paint green $ repeat' 2 $ Join (koch 85 4) (Bend 180)
  draw $ Paint blue $ gosper n
--  draw $ Paint green $ starfish' 2 20
--  draw $ stars 144
  draw $ Paint red $ sierpinski n
--  draw $ Paint red $ marroquin 31
--  draw wow
  flush

--data Sexpr = Symbol String | Number Int | Nil | Cons {car :: Sexpr, cdr :: Sexpr}
data Sexpr = Symbol String | Number Int | FloatS Int Int | Boolean Bool | Nil | Cons {car :: Sexpr, cdr :: Sexpr} | Closure {args :: [String], body :: Sexpr, env :: Local} | Unary {name :: String, func1 :: Sexpr -> Sexpr} | Binary {name :: String, func2 :: Sexpr -> Sexpr -> Sexpr} | Void

--data Sexpr = Symbol String | Number Int | Boolean Bool | Nil | Cons {car :: Sexpr, cdr :: Sexpr} | Closure {args :: [Sexpr], body :: Sexpr} | Unary {name :: String, func1 :: Sexpr -> Sexpr} | Binary {name :: String, func2 :: Sexpr -> Sexpr -> Sexpr} | Void

--data Sexpr = Symbol String | Number Int | Boolean Bool | Nil | Cons {car :: Sexpr, cdr :: Sexpr} | Closure {args :: [(Sexpr, Sexpr)], body :: Sexpr} | Unary {name :: String, func1 :: Sexpr -> Sexpr} | Binary {name :: String, func2 :: Sexpr -> Sexpr -> Sexpr} | Void


type Binding = (String, Sexpr) 
type Frame = [Binding]
type Local = [Frame]
type Global = (Frame, Bool, Local)

instance Show Sexpr where 
    show (Symbol x) =  "symbol " ++ x 
    show (Number x) = "Num" ++ show x 
    show (FloatS x y) = "Float " ++ (show x) ++ "." ++ (show y)
    show Nil = "()"
    show (Cons x y) = "cons" ++ "(" ++ show x ++ showCdr y ++ ")"
    show Void = "#void"
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

cadddr = car . cdr. cdr . cdr
caadddr = car.car . cdr. cdr . cdr
cdadddr =  cdr . car . cdr. cdr . cdr
cddadddr =   cdr . cdadddr


s = do {symb "("; symb ")"; return (Nil)}  +++ do {symb "("; n <- token e; symb ")"; return (n)} +++ do {n <- a; return (n)}
    +++ do {symb "("; n<- token s; symb "."; n2 <- token s; symb ")"; return (Cons n n2)} 

e = do {symb "("; n <- token e; symb ")"; n2 <- token e; return (Cons n n2) }  +++ do {n <- token s; n2 <- token e; return (Cons n n2)}
    +++ do {n <- s; return (Cons n Nil)}

a = do{n <- symbol; return (Symbol n)} +++ do{ n <- number; return (n)}

symbol = do {n <- (first); n2 <- token (many symbolic); return (n:n2)}

symbolic = do {n <- digit; return (intToDigit n)} +++ do {n <- first; return (n)}

number = do {n <- many1 myDigit; symb "."; f <- many1 myDigit; return (FloatS (read n :: Int) (read f :: Int))} +++ do{n <- many1 myDigit; return (Number (read n :: Int))}

myDigit = sat isDigit

-- now parses quotes, need to be able to parse floats like 4.0!
{-
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
-}
misc = do {string "<"; return ('<')} +++
       do {string ">"; return ('>')} +++
       do {string "^"; return ('^')} +++
       do {string "+"; return ('+')} +++
       do {string "-"; return ('-')} +++
       do {string "*"; return ('*')} +++
       do {string "/"; return ('/')} +++
       do {string "="; return ('=')} +++
       do {string "\""; return ('\"')} +++
       do {string "\'"; return ('\'')}

op :: String -> Maybe (Int -> Int -> Int)
op "+" = (Just (+))
op "-" = (Just (-))
op "*" = (Just (*))
op "/" = (Just div)
op _ = Nothing


op2 :: String -> Int -> Int -> Int
op2 "+" = ((+))
op2 "-" = ( (-))
op2 "*" = ( (*))
op2 "/" = (div)

first = misc +++ letter

p st =  fst (head (parse s st))

s3 = Cons (Number 1) (Cons (Number 2) (Cons (Number 3) Nil))
s1 = "(1 2 3)"
s2 = "(define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))"




test = "(define starfish '((to starfish (side angle inc) (repeat 90 (forward side) (right angle) (make angle (+ angle inc)))) (penup) (forward 50) (pendown) (starfish 30 2 20)))"
--test = "((to starfish (side angle inc) (repeat 90 (forward side) (right angle) (make angle (+ angle inc)))) (penup) (forward 50))"

test2 = "(define stars '((to stars (side angle max) (repeat 5 (star side angle max 1) )) (to star (side angle max count) (repeat max (forward (* side count)) (right angle) (make count (+ count 1))))(penup) (forward 50) (pendown) (stars 15 144 8) (penup) (backward 50)))"

test3 = "(define koch '((to koch (n) (if (= n 1) (forward 8) ((koch (- n 1)) (left 60) (koch (- n 1)) (right 120) (koch (- n 1)) (left 60) (koch (- n 1)) ) ) ) (repeat 3 (koch 4)(right 120))))"

test4 = "(define hilbert '((to hilbert (size level parity) (if (> level 0) ((left (* parity 90)) (hilbert size (- level 1) (- parity)) (forward size) (right (* parity 90)) (hilbert size (- level 1) parity) (forward size) (hilbert size (- level 1) parity) (right (* parity 90)) (forward size) (hilbert size (- level 1) (- parity)) (left (* parity 90))) ) ) (hilbert 10 4 1) ))"

test5 = "(define fancy-spiral '((to fancy-spiral (size angle) (if (> size 200) (stop) ) (color (* size (/ 360 200))) (forward size) (right angle) (fancy-spiral (+ size 1) angle) ) (penup) (forward 120) (pendown) (fancy-spiral 0 91) ))"

test6 = "(define tree '((to tree (depth count) (forward (* depth 20)) (right 90) (if (> depth 1) (repeat 5 (push) (left (* count 30)) (color (* 60 count)) (tree (- depth 1) 1) (pop) (make count (+ count 1)) ) ) ) (tree 4 1) ))"

test7 = "(define lissajous '((to lissajous (a b c t) (penup) (setxy (* (cos c) 75) 100) (pendown) (repeat 364 (color t) (setxy (* (cos (+ (* t a) c)) 75) (+ (* (sin (* t b)) 75) 100)) (make t (+ t 1)) ) ) (lissajous 0.1396 -0.12215 0.2094 0)))"

test8 = "(define hexfield '((to hexfield (n c) (if (= n 1) (repeat 6 (forward 20) (left 60) (color (* c 60)) (make c (+ c 1)) ) (repeat 6 (forward 20) (push) (right 180) (hexfield (- n 1) 0) (pop) (left 60) ) ) ) (penup) (forward 100) (pendown) (right 90) (hexfield 3 0) ))"

test9 = "(define broccoli '((to broccoli (x y) (penup) (left 90) (forward 50) (right 90) (pendown) (broccoli1 x y) ) (to broccoli1 (x y) (if (< x y) (stop) ((square x) (forward x) (left 45) (broccoli1 (/ x (sqrt 2)) y) (penup) (backward (/ x (sqrt 2))) (left 45) (pendown) (backward x) ) ) ) (to square (x) (repeat 4 (forward x) (right 90) ) ) (broccoli 100 1)))"

test10 = "(define circles '((to circle (seg clr) (if (< seg 1) (forward 0) (repeat 5 (repeat 8 (make clr (+ clr 10)) (forward seg) (right 9) ) (right 180) (circle (/ seg 2) (+ clr 47)) (right 180) ) ) ) (penup) (setxy -50 200) (pendown) (circle 10 0) ))"

-- TO GET A C GRADE
ex1 = "(forward 50)"
ex2 = "((forward 50) (right 90) (forward 50))"
ex3 = "((forward 50) (left 90) (forward 50))"
ex4 = "((forward 50) (right 90) (backward 50))"
ex5 = "(repeat 5 (penup) (forward 5) (pendown) (forward 5))"
ex9 = "(repeat 5 (forward 50) (right (/ 360 5))) "

testStop = "(repeat 5 (stop) (forward 50)) "
testArgs = "(seg clr)"

-- TEST VARS
aa = caadddr $ p test2 
ab = cdadddr $ p test 
ac = cdadddr $ p test2 

shiftRight90 ::  State Bool (Graphic)
shiftRight90 = do{return(Bend 90)}

-- DISPLAY FUNCTION TO USE:
-- forward will just be straight (backward is just straight with a negative value)
-- left right will be the bend

initialState = ([], True,[])
myDisplay = do
  --draw $ Join (Join (Straight 100) (Bend (90))) (Straight 100)
  --draw $ Straight 100
  --draw $ Join (Bend 90) (Straight 20)
  --draw $ repeat' 5 . Join (Join (Straight 5) (Invisible 0)) . Bend $ 30
  --draw $ Join (Join (Join (Bend (90)) (Straight 50)) (Bend 90)) (Straight 50)
  --draw $ Join (Join (Join (Bend 90) (Straight 50)) (Bend (-89))) (Straight 50)
  --draw $ Join (Bend 90) (fst $ runState (found (p ex5)) True)
  --draw $ Join (Bend 90) (fst $ runState (found (caadddr (p test))) ([], False))
  --draw $ Join (Bend 90) (fst $ runState (found (p ex9)) initialState)
  draw $ Paint green $ (Join (Bend 90) (Join (Paint blue $ (fst $ runState (found (cadddr (p test))) initialState)) (Paint red $ (drawTurtle 10))))
  --draw $ Paint green $ Join (Bend 90) (fst $ runState (found (p test)) initialState)
  flush

drawTurtle sideLength = (Join (Join (Join (Bend (-45)) (Straight sideLength)) (Join (Bend (-90)) (Straight sideLength))) (Join (Bend (-135)) (Straight (sideLength * 1.4))))

foundto (Cons (Symbol "to") x) = (car x , car . cdr $ x)

-- CONVERTERS --
toGLFloat n = read (show n) :: GLfloat


unwrap (Number x) = x 



toList :: Sexpr -> [Sexpr]
toList Nil = []
toList (Cons x y) = x : toList y




toList' :: Sexpr -> [String]
toList' Nil = []
toList' (Cons (Symbol x) y) = x : toList' y




lookupLocal :: String -> Local -> Maybe Sexpr
lookupLocal var [] = Nothing
lookupLocal var (frame:frames) =
    case lookup var frame of
      Nothing -> lookupLocal var frames
      x -> x

lookupEnv :: String -> Local -> State Global Sexpr
lookupEnv var local = do
    case lookupLocal var local of
      Nothing -> do
        (global, penState,local') <- get
        case lookup var global of
                   Just x  -> return x
                   Nothing -> return (Symbol var)
      Just x -> return x



found' :: Sexpr -> State Global Graphic

--found' a b = return(Straight 0)

show' x = show $ head x

<<<<<<< HEAD:cs456/final.hs
--found' (Cons (Symbol "forward") (Cons (Number y) Nil)) = do
--  (envState, penState,local) <- get
--  case penState of 
--    True -> return $ (Straight (toGLFloat y))
--    False -> return $ (Invisible (toGLFloat y))
--found' (Cons (Symbol "forward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do
--  let z = (toGLFloat ((op2 oper) x y))
--  (envState, penState,local) <- get
--  case penState of 
--    True -> return $ (Straight z)
--    False -> return $ (Invisible z)

--found' (Cons (Symbol "forward") (Cons (Symbol oper) Nil))  = do
--  (envState, penState,local) <- get
--  case lookupLocal oper (head local) of
--    (Just x) -> do
--              case penState of 
--                True -> return $ (Straight (toGLFloat (unwrap x)))
--                False -> return $ (Invisible (toGLFloat (unwrap x)))
--    Nothing -> return (Err "Not a symbol")


found' (Cons (Symbol "forward") value) = do
  (envState, penState,local) <- get
  let y = evalProc value (head local)
  case penState of 
    True -> return $ (Straight (toGLFloat y))
    False -> return $ (Invisible (toGLFloat y))





found' (Cons (Symbol "backward") value) = do
  (envState, penState,local) <- get
  let y = evalProc value (head local)
=======
found' (Cons (Symbol "forward") (Cons (Number y) Nil)) = do
  (envState, penState,local) <- get
  case penState of 
    True -> return $ (Straight (toGLFloat y))
    False -> return $ (Invisible (toGLFloat y))
found' (Cons (Symbol "forward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do
  let z = (toGLFloat ((op2 oper) x y))
  (envState, penState,local) <- get
  case penState of 
    True -> return $ (Straight z)
    False -> return $ (Invisible z)

found' (Cons (Symbol "forward") (Cons (Symbol oper) Nil))  = do
  (envState, penState,local) <- get
  case lookupLocal oper local of
    (Just x) -> do
              case penState of 
                True -> return $ (Straight (toGLFloat (unwrap x)))
                False -> return $ (Invisible (toGLFloat (unwrap x)))
    Nothing -> return (Err "Not a symbol")

found' (Cons (Symbol "backward") (Cons (Number y) Nil)) = do
  (envState, penState, local) <- get
>>>>>>> 740cddc3203f1ecab5bd68b5af8dd4e531bfac31:cs456/iffinal.hs
  case penState of 
    True -> return $ (Straight (toGLFloat (-y)))
    False -> return $ (Invisible (toGLFloat (-y)))
found' (Cons (Symbol "backward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do
  let z = (toGLFloat ((op2 oper) x y))
  (envState, penState,local) <- get
  case penState of 
    True -> return $ (Straight (-z))
    False -> return $ (Invisible (-z)) 

found' (Cons (Symbol "backward") (Cons (Symbol oper) Nil)) = do
  (envState, penState,local) <- get
  case lookupLocal oper local of
    (Just x) -> do
              case penState of 
                True -> return $ (Straight (toGLFloat (-(unwrap x))))
                False -> return $ (Invisible (toGLFloat (-(unwrap x))))
    Nothing -> return (Err "Not a symbol")
     
found'  (Cons (Symbol "repeat") (Cons (Number y) x))   = do 
  case y of
    0 -> return (Straight 0)
    _ -> Join <$> found' x <*> (found' (Cons (Symbol "repeat") (Cons (Number (y-1)) x)))
  --return (repeat' y ex)
  --new comm
found' (Cons (Symbol "right") (Cons (Number y) Nil)) = do {return (Bend (toGLFloat (-y)))}
found' (Cons (Symbol "right") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do {return (Bend (toGLFloat (-((op2 oper) x y))))}
found' (Cons (Symbol "right") (Cons (Symbol oper) Nil))  = do
  (envState, penState, local) <- get

  case lookupLocal oper local of
    (Just x) -> do
              case penState of 
                True -> return $ (Bend (toGLFloat (-(unwrap x))))
                False -> return $ (Bend (toGLFloat (-(unwrap x))))
    Nothing -> return (Err "Not a symbol")



found' (Cons (Symbol "left") (Cons (Number y) Nil)) = do {return (Bend (toGLFloat (y)))}
found' (Cons (Symbol "left") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil))= do {return (Bend (toGLFloat ((op2 oper) x y)))}
found' (Cons (Symbol "left") (Cons (Symbol oper) Nil)) = do
  (envState, penState,local) <- get
  case lookupLocal oper local of
    (Just x) -> do
              case penState of 
                True -> return $ (Bend (toGLFloat ((unwrap x))))
                False -> return $ (Bend (toGLFloat ((unwrap x))))
    Nothing -> return (Err "Not a symbol")


found' (Cons (Symbol "make") (Cons (Symbol arg) (Cons expr Nil))) = do
  (ev,pen,local) <- get
  oldValue <- lookupEnv arg local
  let newValue = evalProc expr local
  --let newLocal = localAttach (arg,(Number newValue)) local
  let newLocal = replaceBind ((arg), (Number newValue)) ((arg),oldValue) (head local)
  let newerLocal = localAttach [newLocal] local 
  put(ev,pen,newerLocal)

  return(Err ("Make Found" ++ (show newValue)))

found' (Cons (Symbol "penup") Nil) = do {(envState, _,local) <- get; put (envState, False,local); return (Invisible 0)}
found' (Cons (Symbol "pendown") Nil) = do {(envState, _,local) <- get; put (envState, True,local); return (Straight 0)}
found' (Cons (Symbol "stop") _) = do { return Stop } 
found' (Cons x Nil)  = found' x 
found' (Cons (Symbol fname) args)  = do
  (env,penState,local) <- get
  alpha <- lookupEnv fname []
  let args' = (toList args)
  let newArgs = replaceArgs args' local
  case alpha of 
    Closure params body local' -> do 
                                  let l = zip params newArgs
                                  let local' = l:local'
                                  (envState, penState,_) <- get 
                                  put(envState, penState, local')
                                  found' body  
          
    _ -> return (Err $ show (args'))
found' (Cons x y) = Join <$> found' x <*> found' y


removeLayer :: State Global Graphic
removeLayer = do
          (env,penState,local) <- get
          put (env,penState,(tail local))
          return (Err "This poped the local")




evalProc (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) local = 
                                                                      case op oper of
                                                                        Just f -> (f x y)
                                                                        Nothing -> undefined


evalProc (Cons (Symbol oper) (Cons (Symbol x) (Cons (Number y) Nil))) local = 
                            case op oper of
                                Just f -> case lookupLocal x local of 
                                            Just value -> (f (unwrap value) y)
                                            Nothing -> undefined
                                Nothing -> undefined

evalProc (Cons (Symbol oper) (Cons (Number x) (Cons (Symbol y) Nil))) local = 
                            case op oper of
                                Just f -> case lookupLocal y local of 
                                            Just value -> (f x (unwrap value))
                                            Nothing -> undefined
                                Nothing -> undefined


evalProc (Cons (Symbol oper) (Cons (Symbol x) (Cons (Symbol y) Nil))) local = 
                            case op oper of
                                Just f -> case lookupLocal x local of 
                                            Just value1 -> case lookupLocal y local of 
                                                          Just value -> (f (unwrap value1) (unwrap value))
                                                          Nothing -> undefined
                                            Nothing -> undefined
                                Nothing -> undefined

evalProc (Cons (Symbol x) Nil) local = 
                                 case lookupLocal x local of 
                                                          Just value -> (unwrap value)
                                                          Nothing -> undefined


evalProc (Cons (Number x) Nil) local = x


                                 


--found' (Cons (Symbol "forward") (Cons (Number y) Nil)) = do
--  (envState, penState,local) <- get
--  case penState of 
--    True -> return $ (Straight (toGLFloat y))
--    False -> return $ (Invisible (toGLFloat y))
--found' (Cons (Symbol "forward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do
--  let z = (toGLFloat ((op2 oper) x y))
--  (envState, penState,local) <- get
--  case penState of 
--    True -> return $ (Straight z)
--    False -> return $ (Invisible z)

--found' (Cons (Symbol "forward") (Cons (Symbol oper) Nil))  = do
--  (envState, penState,local) <- get
--  case lookupLocal oper (head local) of
--    (Just x) -> do
--              case penState of 
--                True -> return $ (Straight (toGLFloat (unwrap x)))
--                False -> return $ (Invisible (toGLFloat (unwrap x)))
--    Nothing -> return (Err "Not a symbol")


evalOutside (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) = 
                                                                      case op oper of
                                                                        Just f -> (f x y)
                                                                        Nothing -> undefined


evalOutside (Cons (Number x) Nil) = x

replaceTuple tups old new = map check tups where
    check tup | tup == old = new
              | otherwise  = tup



found :: Sexpr -> State Global Graphic
--found (Cons (Symbol "forward") (Cons (Number y) Nil)) = do
--  (envState, penState) <- get
--  case penState of 
--    True -> return $ (Straight (toGLFloat y))
--    False -> return $ (Invisible (toGLFloat y))
--found (Cons (Symbol "forward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do
--  let z = (toGLFloat ((op2 oper) x y))
--  (envState, penState) <- get
--  case penState of 
--    True -> return $ (Straight z)
--    False -> return $ (Invisible z)

found (Cons (Symbol "forward") value) = do
  let y = evalOutside value
  (envState, penState,_) <- get
  case penState of 
    True -> return $ (Straight (toGLFloat y))
    False -> return $ (Invisible (toGLFloat y))

found (Cons (Symbol "backward") (Cons (Number y) Nil)) = do
  (envState, penState, _) <- get
  case penState of 
    True -> return $ (Straight (toGLFloat (-y)))
    False -> return $ (Invisible (toGLFloat (-y)))
found (Cons (Symbol "backward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do
  let z = (toGLFloat ((op2 oper) x y))
  (envState, penState, _) <- get
  case penState of 
    True -> return $ (Straight (-z))
    False -> return $ (Invisible (-z)) 
     
found  (Cons (Symbol "repeat") (Cons (Number y) x))  = do 
  ex <- found x
  return (repeat' y ex)
found (Cons (Symbol "right") (Cons (Number y) Nil)) = do {return (Bend (toGLFloat (-y)))}
found (Cons (Symbol "right") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do {return (Bend (toGLFloat (-((op2 oper) x y))))}

found (Cons (Symbol "left") (Cons (Number y) Nil)) = do {return (Bend (toGLFloat (y)))}
found (Cons (Symbol "left") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do {return (Bend (toGLFloat ((op2 oper) x y)))}

found (Cons (Symbol "to") (Cons (Symbol name) (Cons args bod))) = do
  (envState, penState, _) <- get
  let binding = (name, Closure (toList' args) bod [])
  let newState = (binding : envState, penState,[])
  put newState
  return (Err "Placed To")
found (Cons (Symbol "penup") Nil) = do {(envState, _,_) <- get; put (envState, False,[]); return (Invisible 0)}
found (Cons (Symbol "pendown") Nil) = do {(envState, _,_) <- get; put (envState, True,[]); return (Straight 0)}
found (Cons (Symbol "stop") _) = do { return Stop }
found (Cons x Nil) = found x
found (Cons (Symbol fname) args) = do
  alpha <- lookupEnv fname []
  let args' = (toList args)
  case alpha of 
    Closure params body local -> do 
                                  let l = zip params args'
                                  let local = l:local
                                  (envState, penState, _) <- get 
                                  put (envState,penState,local)
                                  found' body 
                                  --return(Err "Found Closure")
          
    _ -> return (Err (fname))
found (Cons x y) = Join <$> found x <*> found y
found _  = return(Paint blue $ Straight 10)

body' (Cons (Symbol "to") (Cons (Symbol name) (Cons args bod))) = (toList args)

                                                                     
toNameAndParams (Cons (Symbol "to") x) = (car x , car . cdr $ x)
forwardVal (Cons (Symbol "forward") (Cons (Number y) Nil)) = y
backwardVal (Cons (Symbol "backward") (Cons (Number y) Nil)) = y
leftVal (Cons (Symbol "left") (Cons (Number y) Nil)) = y
rightVal (Cons (Symbol "right") (Cons (Number y) Nil)) = y
repeatVal (Cons (Symbol "repeat") (Cons (Number y) _)) = y


b = (cdr (cdr (cdr (cdr (cdr ab)))))

c = cdr(cdr (b))

localAttach x local = x ++ (tail local)
replaceFrame newBind oldBind local = [replaceBind newBind oldBind frame | frame <- local]
replaceBind newBind oldBind@(z,w) frame = [newBind]++[(x,y)| (x,y) <- frame, (x /= z)]

replaceArgs [] local = []
replaceArgs ((Number x):xs) local = (Number x) : (replaceArgs xs local)
replaceArgs ((Symbol x):xs) local = case lookupLocal x local of
                                    (Just y) -> y : replaceArgs xs local
                                    Nothing  -> (Symbol "dumb") : replaceArgs xs local
replaceArgs _ _ = [(Symbol "failed")]


