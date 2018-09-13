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
import Data.Fixed
type Point = (GLfloat, GLfloat)

instance Num Point where (x, y) + (x', y') = (x + x', y + y')

type Colour = (GLfloat, GLfloat, GLfloat)
data Plumber = Plumber Point GLfloat
data Box = Box GLfloat GLfloat GLfloat GLfloat deriving Show

red   = (1, 0, 0)
green = (0, 1, 0)
blue  = (0, 0, 1)

data Graphic = Straight GLfloat | Invisible GLfloat | Bend GLfloat | Join Graphic Graphic | Fork Graphic Graphic | Err String|Paint Colour Graphic deriving Show

join = foldr1 Join
--fork = foldr1 Fork

renderStraight :: Point -> Point -> StateT Plumber IO ()
renderStraight (x0, y0) (x1, y1) =
    lift $ renderPrimitive Lines $ mapM_ vertex [Vertex2 x0 y0, Vertex2 x1 y1]

degToRad d = (d / 360) * 2 * pi

move l t' = modify (\(Plumber (x, y) t) -> Plumber (x + l * cos t, y + l * sin t) (t + t'))

render :: Graphic -> StateT Plumber IO ()

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
boundingBox (Err _) = forward 0
boundingBox (Straight  length) = forward length
boundingBox (Invisible length) = forward length

-- change move to 10 to see example 3
boundingBox (Bend angle) = move (0) (degToRad angle)

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
  let s = 1 / max dx dy
  let (x, y) = (-s * (xMax + xMin) / 2, -s * (yMax + yMin) / 2)
  runStateT (render (scale s g)) (Plumber (x, y) 0)

repeat' n g = join $ genericTake n (repeat g)

polygon n = repeat' n . Join (Straight 1) . Bend $ 360 / (fromInteger n)


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
  program <- getLine 

  (progname, _) <- getArgsAndInitialize
  createWindow "Haskell Plumbing Graphics"
  counter <- newIORef 1
  let timer = addTimerCallback 1000 $ do {modifyIORef counter increment; postRedisplay Nothing; timer}

  --displayCallback $= display counter
  displayCallback $= myDisplay (selector program)
  --displayCallback $= display
  addTimerCallback 1000 timer

  mainLoop

--display counter = do
--  clear [ColorBuffer]

--  n <- readIORef counter
----  draw $ polygon n
----  draw $ Paint red $ monkey n
--  draw $ Paint green $ repeat' 3 $ Join (koch 60 n) (Bend 120)
----  draw $ Paint green $ repeat' 2 $ Join (koch 85 4) (Bend 180)
--  draw $ Paint blue $ gosper n
----  draw $ Paint green $ starfish' 2 20
----  draw $ stars 144
--  draw $ Paint red $ sierpinski n
----  draw $ Paint red $ marroquin 31
----  draw wow
--  flush

--data Sexpr = Symbol String | Number Int | Nil | Cons {car :: Sexpr, cdr :: Sexpr}
--data Sexpr = Symbol String | Number Int | FloatS Float | Boolean Bool | Nil | Cons {car :: Sexpr, cdr :: Sexpr} | Closure {args :: [String], body :: Sexpr, env :: Local} | Unary {name :: String, func1 :: Sexpr -> Sexpr} | Binary {name :: String, func2 :: Sexpr -> Sexpr -> Sexpr} | Void
data Sexpr = Symbol String | Number GLfloat | FloatS GLfloat | Boolean Bool | Nil | Cons {car :: Sexpr, cdr :: Sexpr} | Closure {args :: [String], body :: Sexpr, env :: Local} | Void 

--data Sexpr = Symbol String | Number Int | Boolean Bool | Nil | Cons {car :: Sexpr, cdr :: Sexpr} | Closure {args :: [Sexpr], body :: Sexpr} | Unary {name :: String, func1 :: Sexpr -> Sexpr} | Binary {name :: String, func2 :: Sexpr -> Sexpr -> Sexpr} | Void

--data Sexpr = Symbol String | Number Int | Boolean Bool | Nil | Cons {car :: Sexpr, cdr :: Sexpr} | Closure {args :: [(Sexpr, Sexpr)], body :: Sexpr} | Unary {name :: String, func1 :: Sexpr -> Sexpr} | Binary {name :: String, func2 :: Sexpr -> Sexpr -> Sexpr} | Void


type Binding = (String, Sexpr) 
type Frame = [Binding]
type Local = [Frame]
type ManyLocal = [Local]
--{turtleX :: Int, turtleY :: Int, turtleAngle :: Int}
type TurtleState = (GLfloat,GLfloat,GLfloat)
-- Global = GlobalFrame, Penstate, LocalState, CurrentTurtleState, StopState, pushState
type Global = (Frame, Bool, ManyLocal,TurtleState,Bool,[TurtleState])



instance Show Sexpr where 
    show (Symbol x) =  "(symbol " ++ x ++")"
    show (Number x) = "(Num" ++ show x  ++ ")"
    show (FloatS x) = "Float " ++ (show x)
    show Nil = "Nil"
    show (Cons x y) = "cons" ++ "(  " ++ show x ++ showCdr y ++ " )"
    show Void = "#void"


showCdr :: Sexpr -> String
showCdr Nil = ""
showCdr (Cons x Nil) = "cons" ++ " " ++ show x ++ (show Nil)
showCdr (Cons x v@(Cons y z)) = "cons" ++ " " ++ show x ++ showCdr v
showCdr (Cons x y) = "cons" ++ " " ++ show x ++ " . " ++ show y
showCdr x = " . " ++ show x



cadddr = car . cdr. cdr . cdr
caadddr = car.car . cdr. cdr . cdr
cdadddr =  cdr . car . cdr. cdr . cdr
cddadddr =   cdr . cdadddr


s = do {symb "("; symb ")"; return (Nil)}  +++ do {symb "("; n <- token e; symb ")"; return (n)} +++ do {n <- a; return (n)}
    +++ do {symb "("; n<- token s; symb "."; n2 <- token s; symb ")"; return (Cons n n2)} 

e = do {symb "("; n <- token e; symb ")"; n2 <- token e; return (Cons n n2) }  +++ do {n <- token s; n2 <- token e; return (Cons n n2)}
    +++ do {n <- s; return (Cons n Nil)}

a = do{ n <- number; return (n)} +++ do{n <- symbol; return (Symbol n)} 

symbol = do {n <- (first); n2 <- token (many symbolic); return (n:n2)}

symbolic = do {n <- digit; return (intToDigit n)} +++ do {n <- first; return (n)}

number = do {n <- many1 myDigit; symb "."; f <- many1 myDigit; return (Number (read (n++ "."++f) :: GLfloat))} +++ 
         do{n <- many1 myDigit; return (Number (read n :: GLfloat))} +++
         do {symb "-"; n <- many1 myDigit; symb "."; f <- many1 myDigit; return (Number (read ("-" ++ n++ "."++f) :: GLfloat))} +++
         do{symb "-"; n <- many1 myDigit; return (Number (read ("-"++n) :: GLfloat))}

myDigit = sat isDigit


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


bool "<" = Just (<)
bool ">" = Just (>)
bool "=" = Just(==)
bool _ = Nothing


oneArg "-" = Just negate 
oneArg "cos" = Just (cos)
oneArg "sin" = Just (sin)

op :: String -> Maybe (GLfloat -> GLfloat-> GLfloat)
op "+" = (Just (+))
op "-" = (Just (-))
op "*" = (Just (*))
op "/" = (Just (/))
op _ = Nothing


op2 :: String -> GLfloat -> GLfloat-> GLfloat
op2 "+" = ((+))
op2 "-" = ( (-))
op2 "*" = ( (*))
op2 "/" = (/)

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

ex13 = "((right 30) (color 60) (forward 100) (right 120) (color 300) (forward 100) (right 120) (color 180) (forward 80) )"
ex14= "((setxy 50 50) (right 45) (forward 40) (setxy 10 100))"

ex10 = "(if (= 10 10)((pendown)(forward 100)(stop))((penup)(backward 100)(stop)))"

ex11 = "((to circle (h r)(repeat 4 (color h) (make r (* (/ h 360) (* 2 3.1416)))(setxy (* (cos r) 50) (+ (* (sin r) 50) 50))(make h (+ h 4)))) (penup)(setxy 50 50)(pendown)(circle 0 0))"

ex16 = "((color 200) (forward 25) (push) (color 0) (right 45) (forward 50)(pop)(forward 25))"

ex17 = "((setxy 3 4) (forward 25))"

selector "ex1" = p ex1
selector "ex2" = p ex2
selector "ex3" = p ex3
selector "ex4" = p ex4
selector "ex5" = p ex5
selector "ex9" = p ex9
selector "ex10" = p ex10
selector "ex11" = p ex11
selector "ex16" = p ex16

selector "ex17" = p ex17

selector "ex13" = p ex13
selector "ex14" = p ex14

selector "test" = cadddr (p test)
selector "test2" = cadddr (p test2)
selector "test3" = cadddr (p test3)
selector "test4" = cadddr (p test4)
selector "test5" = cadddr (p test5)
selector "test6" = cadddr (p test6)
selector "test7" = cadddr (p test7)
selector "test8" = cadddr (p test8)
selector "test9" = cadddr (p test9)
selector "test10" = cadddr (p test10)



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

-- theta = 90 - currentAngle, dx = x2-x1, dy = y2-y1
testSetXY (dx, dy, theta) penState = 
              case penState of 
                False -> Join (Join (Join (Join (Bend (theta)) (Invisible dy)) (Bend (-90))) (Invisible dx)) (Bend (90-theta))
                True -> Join (Join (Join (Join (Bend (theta)) (Straight dy)) (Bend (-90))) (Straight dx)) (Bend (90-theta))


setXY (dx, dy, theta) penState = 
                case penState of 
                  False -> (Join (Join (Join (Join (Bend (theta)) (Bend(at(dy/dx)))) (Invisible (hypt dx dy))) (Bend (-(at(dy/dx))))) (Bend(90-theta)))
                  True -> (Join (Join (Join (Join (Bend (theta)) (Bend(at(dy/dx)))) (Straight (hypt dx dy))) (Bend (-(at(dy/dx))))) (Bend(90-theta)))


degrees x = (x*180)/pi

at = degrees.atan
hypt x y = sqrt((x*x) + (y*y))
{-
-- h is angle
findColor h
  | ((h >= 0) && (h < 60)) = (1*255, (findX h)*255, 0)
  | ((h >= 60) && (h < 120)) = ((findX h)*255, 1*255, 0)
  | ((h >= 120) && (h < 180)) = (0, 1*255, (findX h)*255)
  | ((h >= 180) && (h < 240)) = (0, (findX h)*255, 1*255)
  | ((h >= 240) && (h < 300)) = ((findX h)*255, 0, 1*255)
  | ((h >= 300) && (h < 360)) = (1*255, 0, (findX h)*255)
  | otherwise = (255, 255, 255)

-- helper function for findColor
findX h = 1 - abs( (mod' (h/60) 2) - 1)
-}



cosd = cos . degToRad

whichH h 
  | (h < 0) = 0
  | ((0 <= h) && (h < 120)) = 1
  | ((120 <= h) && (h < 240)) = 2
  | ((240 <= h) && (h < 360)) = 3
  | otherwise = 4
  
findColor :: GLfloat -> (GLfloat, GLfloat, GLfloat)
findColor h = case (whichH h) of
  0 -> findColor (h + 360)
  1 -> let r = (1 + ((cosd h) / (cosd (60 -h)))) / (sqrt 3)
           g = ((sqrt 3) - r)
            in (r, g, 0)
  2 -> let g = (1 + (cosd (h-120) / (cosd (180 -h)))) / (sqrt 3)
           b = (sqrt 3) - g
            in (0, g, b)
  3 -> let b = ((1 + (cosd (h-240) / cosd (300 -h))))/ (sqrt 3)
           r = (sqrt 3) - b
            in (r, 0, b)
  4 -> findColor (h - 360)


-- track the position of the turtle: w is the current angle (with respect to angle 0. NOTE: we always bend 90 before drawing, so our initial state has angle=90 instead of 0), 
--                                   (x1, y1) is the current position, 
--                                   l is how much forward (parsed by right or left)
-- When a right or left is seen, don't have to call track x,y. Just update the state
trackXY (x1, y1, w) l = (x2, y2, w)
  where x2 = x1 + ((cos (degToRad w)) * l)
        y2 = y1 + ((sin (degToRad w)) * l)

trackAngle (x, y, w) t = (x, y, w + t)

initialTurtle = (0,0,90)

-- true pendown false penup
initialState = ([], True,[],initialTurtle,False,[])
myDisplay value= do
  clear [ColorBuffer]
  --draw $ Join (Join (Straight 100) (Bend (90))) (Straight 100)
  --draw $ Straight 100
  --draw $ Join (Bend 90) (Straight 20)
  --draw $ repeat' 5 . Join (Join (Straight 5) (Invisible 0)) . Bend $ 30
  --draw $ Join (Join (Join (Bend (90)) (Straight 50)) (Bend 90)) (Straight 50)
  --draw $ Join (Join (Join (Bend 90) (Straight 50)) (Bend (-89))) (Straight 50)
  --draw $ Join (Bend 90) (fst $ runState (found (p ex5)) True)
  --draw $ Join (Bend 90) (fst $ runState (found (caadddr (p test))) ([], False))
  --draw $ Join (Bend 90) (fst $ runState (found (p ex9)) initialState)
  --draw $ (Join (Bend 90)(fst $ runState (found (value)) initialState) )
  draw $ (Join (Bend 90) (Join (drawTurtle 0.5) (Join (fst $ runState (found (value)) initialState) (drawTurtle 0.5))))
  flush

--drawTurtle sideLength = (Join (Join (Join (Bend (120)) (Straight sideLength)) (Join (Bend (120)) (Straight sideLength))) (Join (Bend (120)) (Straight (sideLength * 1.0))))

drawTurtle sideLength = Join (Join (Join (Join (Join (Join (Straight (sideLength / 2)) (Bend 120)) (Straight sideLength)) (Bend 120)) (Straight sideLength)) (Bend 120)) (Straight (sideLength / 2))

foundto (Cons (Symbol "to") x) = (car x , car . cdr $ x)

-- CONVERTERS --
toGLFloat n = read (show n) :: GLfloat

unwrap (Number x) = x 
unwra (Symbol x) = x 

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
        (global, penState,local',_,_,_) <- get
        case lookup var global of
                   Just x  -> return x
                   Nothing -> return (Symbol var)
      Just x -> return x

found' :: Sexpr -> State Global Graphic


cosw (Just x) = unwrap x
show' x = show $ head x


found' (Cons (Symbol "forward") value) = do
  (envState, penState,local,currentTurt,stop,pushTurt) <- get
  let y = evalProc value (head local)
  let newTurt = trackXY currentTurt y
  put (envState, penState, local, newTurt, stop, pushTurt)
  case stop of
    False -> case penState of 
              True -> return $ (Straight (y))
              False -> return $ (Invisible (y))
    True -> return (Invisible y)



  



found' (Cons (Symbol "backward") value) = do
  (envState, penState,local,currentTurt,stop,pushTurt) <- get
  let y = evalProc value (head local)
  let newTurt = trackXY currentTurt y
  put (envState, penState, local, newTurt, stop, pushTurt)
  case stop of
    False -> case penState of 
              True -> return $ (Straight (-y))
              False -> return $ (Invisible (-y))
    True -> return (Invisible (-y))


     
found'  (Cons (Symbol "repeat") (Cons (Symbol sym) x))   = do 
  (evn, penState,local,_,_,_) <- get
  case lookupLocal sym (head local) of
   Just (Number y) -> Join <$> found' x <*> found' (Cons (Symbol "repeat") (Cons (Number (y-1)) x))
   _ -> return (Err "repeat faild")

found'  (Cons (Symbol "repeat") (Cons (Number y) x))   = do 
  case y of
    0 -> return (Straight 0)
    _ -> Join <$> found' x <*> found' (Cons (Symbol "repeat") (Cons (Number (y-1)) x))
 




found' (Cons (Symbol "right") value) = do
    (envState, penState,local,currentTurt,stop,pushTurt) <- get
    let y = evalProc value (head local)
    let newTurt = trackAngle currentTurt (-y) 
    put (envState, penState, local, newTurt, stop, pushTurt)
    return (Bend (toGLFloat (-y)))


found' (Cons (Symbol "left") value) = do
  (envState, penState,local,currentTurt,stop,pushTurt) <- get
  let y = evalProc value (head local)
  let newTurt = trackAngle currentTurt y 
  put (envState, penState, local, newTurt, stop, pushTurt)
  return (Bend (toGLFloat (y)))

found' (Cons (Symbol "color") value) = do
  (envState, penState,local,_,_,_) <- get
  let y = evalProc value (head local)
  let color = findColor y
  return (Paint color $ (Err $ show color))
  --return (Err $ "This is the value at color"++ (show color))

found' (Cons (Symbol "push") Nil) = do
    (envState, penState,local,currentTurt,stop,stack) <- get
    let newStack = currentTurt:stack
    put(envState, penState,local,currentTurt,stop,newStack)
    return(Err "pushed")

found' (Cons (Symbol "pop") Nil) = do
    (envState, penState,local,currentTurt@(fstq,sndq,old),stop,stack) <- get
    let newStack = (tail stack)
    let newTurt@(x,y,trd) = (head stack)
    let dx = x - fstq
    let dy = y - sndq
    let t = 90 - old
    put(envState, penState,local,newTurt,stop,newStack)
    return (Join (testSetXY (dx,dy,t) penState) (Err "poped"))


found' (Cons (Symbol "setxy") (Cons value (Cons (value2) Nil))) = do
    (envState, penState,local,currentTurt@(fstq,sndq,trd),stop,pushTurt) <- get
    let x = evalProc value (head local)
    let y = evalProc value2 (head local)
    let dx = x - fstq
    let dy = y - sndq
    let t = 90 - trd
    let newTurt = (x,y,trd)
    put (envState, penState,local,newTurt,stop,pushTurt)
    let w = lookupLocal "r" (head local)
    return (Join (testSetXY (dx, dy, t) penState) (Err $ "setxy"++ ( show newTurt) ++ show y))
    --return (Err $"This is the setxy" ++(show newTurt) ++ "   x " ++ (show x) ++ " y " ++(show y))


found' (Cons (Symbol "make") (Cons (Symbol arg) (Cons expr Nil))) = do
  (ev,pen,local,lastTurtle,stop,lastPush) <- get
  oldValue <- lookupEnv arg (head local)
  let newValue = evalProc expr (head local)
  --let newLocal = localAttach (arg,(Number newValue)) local
  let newLocal = replaceBind ((arg), (Number newValue)) ((arg),oldValue) (head (head local))
  let newerLocal = (localAttach [newLocal] (head local)) : (tail local)
  put(ev,pen,newerLocal,lastTurtle,stop,lastPush)

  return(Err ("Make Found" ++ (show arg) ++(show newLocal)))

found' (Cons (Symbol "if") (Cons value (Cons exp1 Nil))) = do
    (ev,pen,local,_,_,_) <- get
    case evalBool value (head local) of
      True -> found' exp1
      False -> return(Err "if evaluated to false")

found' (Cons (Symbol "if") (Cons value (Cons exp1 exp2))) = do
    (ev,pen,local,_,_,_) <- get
    case evalBool value (head local) of
      True -> found' exp1
      False -> found' exp2

found' (Cons (Symbol "stop") Nil) = do
    (ev,pen,local,turtlePos,_,pushPos) <- get
    put (ev,pen,local,turtlePos,True,pushPos)
    return(Err "stopped")

found' (Cons (Symbol "penup") Nil) = do {(envState, _,local,lastTurtle,stop,lastPush) <- get; put (envState, False,local,lastTurtle,stop,lastPush); return (Err "penup")}
found' (Cons (Symbol "pendown") Nil) = do {(envState, _,local,lastTurtle,stop,lastPush) <- get; put (envState, True,local,lastTurtle,stop,lastPush); return (Err "pendown")}
found' (Cons x Nil)  = found' x 
found' (Cons (Symbol fname) args)  = do
  (env,penState,local,lastTurtle,stop,lastPush) <- get
  alpha <- lookupEnv fname []
  let args' = (toList args)
  let newArgs = replaceArgs args' (head local)
  case alpha of 
    Closure params body local' -> do 
                                  let l = zip params newArgs
                                  let local' = [(l:(head local'))] ++ local
                                  put(env, penState, local',lastTurtle,stop,lastPush)
                                  Join <$> return(Err ("this is newArgs"++ (show l))) <*> (Join <$> found' body <*> removeLayer )
          
    _ -> return (Err $ "didn't return closure" ++ (show (args')))
found' (Cons x y) = do
    (env,penState,local,lastTurtle,stop,lastPush) <- get
    case stop of 
        False -> Join <$> found' x <*> found' y 
        True  -> return (Err "stopped ")

removeLayer :: State Global Graphic
removeLayer = do
          (env,penState,local,lastTurtle,stop,lastPush) <- get
          put (env,penState,(tail local),lastTurtle,stop,lastPush)
          return (Err "This poped the local")



--Cons(Symbol forward (Cons (Cons (Symbol *) (Cons (Symbol count) (Cons (Number 1) Nil))) Nil)



--Cons (Cons (Symbol *) (Cons (Symbol side) Cons (Symbol count)())) 
--evalProc (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) local = 
--                                                                      case op oper of
--                                                                        Just f -> (f x y)
--                                                                        Nothing -> undefined

--evalProc (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)local = 
--                                                                      case op oper of
--                                                                        Just f -> (f x y)
--                                                                        Nothing -> undefined

--evalProc (Cons (Symbol oper) (Cons (Symbol x) (Cons (Number y) Nil))) local = 
--                            case op oper of
--                                Just f -> case lookupLocal x local of 
--                                            Just value -> (f (unwrap value) y)
--                                            Nothing -> undefined
--                                Nothing -> undefined


--evalProc (Cons (Cons (Symbol oper) (Cons (Symbol x) (Cons (Number y) Nil)))Nil) local = 
--                            case op oper of
--                                Just f -> case lookupLocal x local of 
--                                            Just value -> (f (unwrap value) y)
--                                            Nothing -> undefined
--                                Nothing -> undefined

--evalProc (Cons (Symbol oper) (Cons (Number x) (Cons (Symbol y) Nil))) local = 
--                            case op oper of
--                                Just f -> case lookupLocal y local of 
--                                            Just value -> (f x (unwrap value))
--                                            Nothing -> undefined
--                                Nothing -> undefined


--evalProc (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Symbol y) Nil)))Nil) local = 
--                            case op oper of
--                                Just f -> case lookupLocal y local of 
--                                            Just value -> (f x (unwrap value))
--                                            Nothing -> undefined
--                                Nothing -> undefined

--evalProc (Cons (Symbol oper) (Cons (Symbol x) (Cons (Symbol y) Nil))) local = 
--                            case op oper of
--                                Just f -> case lookupLocal x local of 
--                                            Just value1 -> case lookupLocal y local of 
--                                                          Just value -> (f (unwrap value1) (unwrap value))
--                                                          Nothing -> undefined
--                                            Nothing -> undefined
--                                Nothing -> undefined


--evalProc (Cons (Cons (Symbol oper) (Cons (Symbol x) (Cons (Symbol y) Nil)))Nil) local = 
--                            case op oper of
--                                Just f -> case lookupLocal x local of 
--                                            Just value1 -> case lookupLocal y local of 
--                                                          Just value -> (f (unwrap value1) (unwrap value))
--                                                          Nothing -> undefined
--                                            Nothing -> undefined
--                                Nothing -> undefined
--evalProc (Cons (Symbol x) Nil) local = 
--                                 case lookupLocal x local of 
--                                                          Just value -> (unwrap value)
--                                                          Nothing -> undefined

----scons  cons  (symbol *)cons (symbol size)cons cons  (symbol /)cons (Num360.0)cons (Num200.0)Nil Nil  \)

--          --(Cons (Symbol oper) (Cons (Symbol x) (Cons value Nil)))

--evalProc (Cons (Cons (Symbol oper) (Cons (Symbol x) (Cons value Nil)))Nil) local = 
--                                case op oper of
--                                Just f -> case lookupLocal x local of 
--                                            Just v -> (f (unwrap v) (evalProc value local))
--                                            Nothing -> undefined
--                                Nothing -> undefined

--evalProc (Cons (Cons (Symbol oper) (Cons (Cons value Nil) (Symbol x) ))Nil) local = 
--                                case op oper of
--                                Just f -> case lookupLocal x local of 
--                                            Just v -> (f (evalProc value local) (unwrap v))
--                                            Nothing -> undefined
--                                Nothing -> undefined

--evalProc (Cons (Cons (Symbol oper) (Cons (Number x) (Cons value Nil)))Nil) local = 
--                                case op oper of
--                                Just f -> (f x (evalProc value local))
--                                Nothing -> undefined


--evalProc (Cons (Cons (Symbol oper) (Cons (Cons value Nil) (Number x)))Nil) local = 
--                                case op oper of
--                                Just f -> (f  (evalProc value local) x)
--                                Nothing -> undefined

--evalProc (Cons (Symbol oper) (Cons (Symbol x) Nil)) local = 
--                            case oneArg oper of
--                                Just f -> case lookupLocal x local of 
--                                            Just value -> (f (unwrap value))
--                                            Nothing -> undefined
--                                Nothing -> undefined

--evalProc (Cons (Number x) Nil) local = x

--evalProc _  local= 1000                





--evalProc (Cons (Cons (Symbol oper) (Cons v Nil)) Nil) local = 
--                                      case oneArg oper of
--                                          Just f -> (f (evalProc v local))
--                                          Nothing -> undefined
                                 

--evalProc (Cons (Cons (Symbol oper) (Cons value1 value2)) Nil) local = 
--                                        case op oper of
--                                            Just f -> (f (evalProc value1 local) (evalProc value2 local))
--                                            Nothing -> undefined

--evalProc (Cons (Symbol oper) (Cons value1 value2)) local = 
--                                          case op oper of
--                                            Just f -> (f (evalProc value1 local) (evalProc value2 local))
--                                            Nothing -> undefined

--evalProc (Cons (Symbol x) Nil) local = 
--                                 case lookupLocal x local of 
--                                                          Just value -> (unwrap value)
--                                                          Nothing -> undefined
--evalProc (Cons (Number x) Nil) local = x

--evalProc (Number x) local = x
--evalProc (Symbol x) local = 
--                                 case lookupLocal x local of 
--                                                          Just value -> (unwrap value)
--                                                          Nothing -> undefined

--evalProc (Cons (Symbol op2 ) (Cons (Cons (Symbol op1) (Cons (Cons (Symbol "sin") (Cons (Symbol x) Nil)) (Cons (Number z) Nil))) (Cons (Number y) Nil))) local = 
--                                        case op op1 of
--                                          Just f ->  (f (sin (evalProc (Symbol x) local)) z) + 50
--                                          Nothing -> undefined

evalProc (Cons (Cons (Symbol oper) (Cons v Nil)) Nil) local = 
                                      case oneArg oper of
                                          Just f -> (f $ evalProc v local)
                                          Nothing -> undefined
                                 
evalProc (Cons (Symbol oper) (Cons v Nil)) local = 
                                      case oneArg oper of
                                          Just f -> (f $ evalProc v local)
                                          Nothing -> undefined

evalProc (Cons (Cons (Symbol oper) (Cons value1 value2)) Nil) local  = 
                                        case op oper of
                                            Just f -> (f (evalProc value1 local) (evalProc value2 local))
                                            Nothing -> undefined

evalProc (Cons (Symbol oper) (Cons value1 value2)) local  = 
                                          case op oper of
                                            Just f -> (f (evalProc value1 local) (evalProc value2 local)) 
                                            Nothing -> undefined

evalProc (Cons (Number x) Nil) local =  x

evalProc (Cons (Symbol x) Nil) local = 
                                 case lookupLocal x local of 
                                                          Just value -> (unwrap value)
                                                          Nothing -> undefined

evalProc (Symbol x) local = 
                                 case lookupLocal x local of 
                                                          Just value -> (unwrap value)
                                                          Nothing -> undefined
evalProc (Number x) local =  x



-- Start of EvalBool for if statement

evalBool (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) local = 
                                                                      case bool oper of
                                                                        Just f -> (f x y)
                                                                        Nothing -> undefined

evalBool (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)local = 
                                                                      case bool oper of
                                                                        Just f -> (f x y)
                                                                        Nothing -> undefined

evalBool(Cons (Symbol oper) (Cons (Symbol x) (Cons (Number y) Nil))) local = 
                            case bool oper of
                                Just f -> case lookupLocal x local of 
                                            Just value -> (f (unwrap value) y)
                                            Nothing -> undefined
                                Nothing -> undefined


evalBool (Cons (Cons (Symbol oper) (Cons (Symbol x) (Cons (Number y) Nil)))Nil) local = 
                            case bool oper of
                                Just f -> case lookupLocal x local of 
                                            Just value -> (f (unwrap value) y)
                                            Nothing -> undefined
                                Nothing -> undefined

evalBool (Cons (Symbol oper) (Cons (Number x) (Cons (Symbol y) Nil))) local = 
                            case bool oper of
                                Just f -> case lookupLocal y local of 
                                            Just value -> (f x (unwrap value))
                                            Nothing -> undefined
                                Nothing -> undefined


evalBool (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Symbol y) Nil)))Nil) local = 
                            case bool oper of
                                Just f -> case lookupLocal y local of 
                                            Just value -> (f x (unwrap value))
                                            Nothing -> undefined
                                Nothing -> undefined

evalBool (Cons (Symbol oper) (Cons (Symbol x) (Cons (Symbol y) Nil))) local = 
                            case bool oper of
                                Just f -> case lookupLocal x local of 
                                            Just value1 -> case lookupLocal y local of 
                                                          Just value -> (f (unwrap value1) (unwrap value))
                                                          Nothing -> undefined
                                            Nothing -> undefined
                                Nothing -> undefined


evalBool (Cons (Cons (Symbol oper) (Cons (Symbol x) (Cons (Symbol y) Nil)))Nil) local = 
                            case bool oper of
                                Just f -> case lookupLocal x local of 
                                            Just value1 -> case lookupLocal y local of 
                                                          Just value -> (f (unwrap value1) (unwrap value))
                                                          Nothing -> undefined
                                            Nothing -> undefined
                                Nothing -> undefined






--evalBoolOutside (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil)))  = 
--                                                                      case bool oper of
--                                                                        Just f -> (f x y)
--                                                                        Nothing -> undefined

--evalOutside (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) = 
--                                                                      case op oper of
--                                                                        Just f -> (f x y)
--                                                                        Nothing -> undefined


--evalOutside (Cons (Number x) Nil) = x

--evalOutside (Number x) = x




ex7 = "(* (cos (+ (* 3 3) 3)) 75)"

mult = (read "50" :: GLfloat) + ((sin (read "0.06981" :: GLfloat)) * (read "50" :: GLfloat))

evalOutside (Cons (Cons (Symbol oper) (Cons v Nil)) Nil) = 
                                      case oneArg oper of
                                          Just f -> (f $ evalOutside v)
                                          Nothing -> undefined
                                 
evalOutside (Cons (Symbol oper) (Cons v Nil)) = 
                                      case oneArg oper of
                                          Just f -> (f $ evalOutside v)
                                          Nothing -> undefined

evalOutside (Cons (Cons (Symbol oper) (Cons value1 value2)) Nil)  = 
                                        case op oper of
                                            Just f -> (f (evalOutside value1) (evalOutside value2))
                                            Nothing -> undefined

evalOutside (Cons (Symbol oper) (Cons value1 value2))  = 
                                          case op oper of
                                            Just f -> (f (evalOutside value1) (evalOutside value2))
                                            Nothing -> undefined

evalOutside (Cons (Number x) Nil)  =  x

evalOutside (Number x) =  x


--test for ligsfsfs
--evalOutside1 (Cons (Symbol "*") (Cons (Cons (Symbol "cos") (Cons (Number 0.2094) Nil)) (Cons (Number 75.0) Nil)))
--Cons ((symbol +) cons cons(  (symbol *)cons cons(  (symbol sin)cons (symbol r)Nil )cons (Num50.0)Nil )
--evalOutside1 (Cons (Symbol "+") (Cons (Cons (Symbol "*") (Cons (Cons (Symbol "sin") (Cons (Number 0.069756) Nil)) (Cons (Number 50.0) Nil)) (Cons (Number 50.0) Nil))))

--evalOutside1 (Cons (Symbol "+") (Cons  (Cons (Symbol "*") (Cons  (Cons (Symbol "sin") (Cons (Number 0.069756)Nil)) (Cons (Number 50.0) Nil))) (Cons (Number 50.0) Nil)))

--evalOutside1 (Cons (Symbol "+" ) (Cons (Cons (Symbol "*") (Cons (Cons (Symbol "sin") (Cons (Number 0.069756) Nil)) (Cons (Number 50.0) Nil)))(Cons (Number 50.0) Nil) ))

evalOutside1 (Cons (Cons (Symbol oper) (Cons v Nil)) Nil) = 
                                      case oneArg oper of
                                          Just f -> Symbol (unwra $ evalOutside1 v)
                                          Nothing -> (Symbol $ "failed0 " ++ oper)
                                 
evalOutside1 (Cons (Symbol oper) (Cons v Nil)) = 
                                      case oneArg oper of
                                          Just f -> Symbol (unwra $ evalOutside1 v)
                                          Nothing -> (Symbol $ "failed0 " ++ oper)

evalOutside1 (Cons (Cons (Symbol oper) (Cons value1 value2)) Nil)  = 
                                        case op oper of
                                            Just f -> Symbol  $ (unwra $ evalOutside1 value1) ++ (unwra $ evalOutside1 value2)
                                            Nothing -> (Symbol $ "failed1 " ++ oper)

evalOutside1 (Cons (Symbol oper) (Cons value1 value2))  = 
                                          case op oper of
                                            Just f -> Symbol  $ "(Case 3"++ (show oper) ++ " "++ (unwra $ evalOutside1 value1) ++ " value  "++ (unwra $ evalOutside1 value2) ++")"
                                            Nothing -> (Symbol $ "failed2 " ++ oper ++ " sfs  ")

evalOutside1 (Cons (Number x) Nil)  = Symbol $ show x

evalOutside1 (Number x) = Symbol$ show x


--evalOutside1 value = value
z = cadddr $ p test7


evalBoolOutside (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil)))  = 
                                                                      case bool oper of
                                                                        Just f -> (f x y)
                                                                        Nothing -> undefined



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
  (envState, penState,local,currentTurt,stop,pushTurt) <- get
  let newTurt = trackXY currentTurt y
  put(envState, penState,local,newTurt,stop,pushTurt)

  case stop of
    False -> case penState of 
              True -> return $ (Straight (y))
              False -> return $ (Invisible (y))
    True -> return (Invisible y)
  
found (Cons (Symbol "color") value) = do
  let y = evalOutside value
  let color = findColor y
  return (Paint color $ (Straight 0))


found (Cons (Symbol "backward") value) = do
  let y = evalOutside value
  (envState, penState,local,currentTurt,stop,pushTurt) <- get
  let newTurt = trackXY currentTurt y
  put(envState, penState,local,newTurt,stop,pushTurt)
  case stop of
    True -> case penState of 
              True -> return $ (Straight (-y))
              False -> return $ (Invisible (-y))
    False -> return (Invisible (-y))
     
found  (Cons (Symbol "repeat") (Cons (Number y) x))   = do 
  case y of
    0 -> return (Straight 0)
    _ -> Join <$> found' x <*> found' (Cons (Symbol "repeat") (Cons (Number (y-1)) x))


found (Cons (Symbol "right") (Cons (Number y) Nil)) = do {return (Bend ((-y)))}
found (Cons (Symbol "right") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do {return (Bend ((-((op2 oper) x y))))}

found (Cons (Symbol "left") (Cons (Number y) Nil)) = do {return (Bend ((y)))}
found (Cons (Symbol "left") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do {return (Bend (((op2 oper) x y)))}


found (Cons (Symbol "push") Nil) = do
    (envState, penState,local,currentTurt,stop,stack) <- get
    let newStack = currentTurt:stack
    put(envState, penState,local,currentTurt,stop,newStack)
    return(Err $ "pushed" ++ show(currentTurt) )

found (Cons (Symbol "pop") Nil) = do
    (envState, penState,local,currentTurt@(fstq,sndq,old),stop,stack) <- get
    let newStack = (tail stack)
    let newTurt@(x,y,trd) = (head stack)
    let dx = x - fstq
    let dy = y - sndq
    let t = 90 - old
    put(envState, penState,local,newTurt,stop,newStack)
    return (Paint green $ (Join (testSetXY (dx,dy,t) penState) (Err $ "poped" ++ (show newTurt))))


found (Cons (Symbol "setxy") (Cons value (Cons (value2) Nil))) = do
    (envState, penState,local,currentTurt@(fst,snd,trd),stop,pushTurt) <- get
    let x = evalOutside value
    let y = evalOutside value2
    let dx = x - fst
    let dy = y - snd
    let t = 90 - trd
    let newTurt = (x,y,trd)
    put (envState, penState,local,newTurt,stop,pushTurt)
    --return (Err $ (show currentTurt) ++ "New Turt " ++ (show newTurt))
    return (Join (testSetXY (dx, dy, t) penState)(Err $ (show dx) ++ show (dy)))

found (Cons (Symbol "stop") Nil) = do
    (ev,pen,_,turtlePos,_,pushPos) <- get
    put (ev,pen,[],turtlePos,True,pushPos)
    return(Err "stopped")

found (Cons (Symbol "to") (Cons (Symbol name) (Cons args bod))) = do
  (envState, penState, _,lastTurtle,stop,lastPush) <- get
  let binding = (name, Closure (toList' args) bod [])
  let newState = (binding : envState, penState,[],lastTurtle,stop,lastPush)
  put newState
  return (Err "Placed To")


found (Cons (Symbol "if") (Cons value (Cons exp1 Nil))) = do
    (ev,pen,local,_,_,_) <- get
    case evalBoolOutside value of
      True -> found exp1
      False -> return(Err "if evaluated to false")

found (Cons (Symbol "if") (Cons value (Cons exp1 exp2))) = do
    case evalBoolOutside value of
      True -> found exp1
      False -> found exp2
found (Cons (Symbol "penup") Nil) = do {(envState, _,_,lastTurtle,stop,lastPush) <- get; put (envState, False,[],lastTurtle,stop,lastPush); return (Invisible 0)}
found (Cons (Symbol "pendown") Nil) = do {(envState, _,_,lastTurtle,stop,lastPush) <- get; put (envState, True,[],lastTurtle,stop,lastPush); return (Straight 0)}
found (Cons x Nil) = found x
found (Cons (Symbol fname) args) = do
  alpha <- lookupEnv fname []
  let args' = (toList args)
  case alpha of 
    Closure params body local -> do 
                                  let l = zip params args'
                                  let local = [l:(head local)] 
                                  (envState, penState, _,lastTurtle,stop,lastPush) <- get 
                                  put (envState,penState,local,lastTurtle,stop,lastPush)
                                  Join <$> return (Err $  "l is this " ++(show l)) <*> (Join <$> found' body <*> removeLayer )
                                  --return(Err "Found Closure")
          
    _ -> return (Err ("found Fname"++ fname))
found (Cons x y) = Join <$> found x <*> found y
found _  = return(Paint blue $ Straight 10)

body' (Cons (Symbol "to") (Cons (Symbol name) (Cons args bod))) = (toList args)


                                                                     
toNameAndParams (Cons (Symbol "to") x) = (car x , car . cdr $ x)
forwardVal (Cons (Symbol "forward") (Cons (Number y) Nil)) = y
backwardVal (Cons (Symbol "backward") (Cons (Number y) Nil)) = y
leftVal (Cons (Symbol "left") (Cons (Number y) Nil)) = y
rightVal (Cons (Symbol "right") (Cons (Number y) Nil)) = y
repeatVal (Cons (Symbol "repeat") (Cons (Number y) _)) = y

ia (Cons (Symbol "if") (Cons value (Cons exp1 exp2)))  = evalBool value []

b = (cdr (cdr (cdr (cdr (cdr ab)))))

c = cdr(cdr (b))

localAttach x local = x ++ (tail local)
replaceFrame newBind oldBind local = [replaceBind newBind oldBind frame | frame <- local]
replaceBind newBind oldBind@(z,w) frame = [newBind]++[(x,y)| (x,y) <- frame, (x /= z)]

replaceArgs [] local = []
replaceArgs ((Number x):xs) local = (Number x) : (replaceArgs xs local)
replaceArgs (a@(Cons x y):xs) local = (Number (evalProc a local)) : (replaceArgs xs local)
replaceArgs ((Symbol x):xs) local = case lookupLocal x local of
                                    (Just y) -> y : replaceArgs xs local
                                    Nothing  -> (Symbol "dumb") : replaceArgs xs local
replaceArgs x _ = [(Symbol $ show x)]




