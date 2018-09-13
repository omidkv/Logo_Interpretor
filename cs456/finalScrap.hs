-- PATTERN MATCH TO EVALUATE THE PARSED LOGO CODE --
{-
found :: Sexpr -> State Bool (Graphic)
found (Cons (Symbol "forward") (Cons (Number y) Nil)) = do
  penState <- get
  case penState of 
    True -> return $ (Straight (toGLFloat y))
    False -> return $ (Invisible (toGLFloat y))
found (Cons (Symbol "forward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do
  let z = (toGLFloat ((op oper) x y))
  penState <- get
  case penState of 
    True -> return $ (Straight z)
    False -> return $ (Invisible z)

found (Cons (Symbol "backward") (Cons (Number y) Nil)) = do
  penState <- get
  case penState of 
    True -> return $ (Straight (toGLFloat (-y)))
    False -> return $ (Invisible (toGLFloat (-y)))
found (Cons (Symbol "backward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do
  let z = (toGLFloat ((op oper) x y))
  penState <- get
  case penState of 
    True -> return $ (Straight (-z))
    False -> return $ (Invisible (-z)) 
     
found  (Cons (Symbol "repeat") (Cons (Number y) x))  = do 
  ex <- found x
  return (repeat' y ex)
found (Cons (Symbol "right") (Cons (Number y) Nil)) = do {return (Bend (toGLFloat (-y)))}
found (Cons (Symbol "right") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do {return (Bend (toGLFloat (-((op oper) x y))))}

found (Cons (Symbol "left") (Cons (Number y) Nil)) = do {return (Bend (toGLFloat (y)))}
found (Cons (Symbol "left") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do {return (Bend (toGLFloat ((op oper) x y)))}

found (Cons (Symbol "penup") Nil) = do {put True; return (Invisible 0)}
found (Cons (Symbol "pendown") Nil) = do {put False; return (Straight 0)}
found (Cons x Nil) = found x
found (Cons x y) = Join <$> found x <*> found y
-}





-- FOR BASIC: Assume no floats, then the scheme parser can parse these string values
--f = "(define foo \'((100 input \"what is the value of a\" a ) (110 input \"what is the value of b\" b ) (120 input \"what is the value of b\" c ) (130 let d = ((b * b) - (4 * (a * c)))) (140 print d) (150 end)))"

--test = "(define foo \'((100 input \"what is the value of a\" a ) (110 input \"what is the value of b\" b ) (120 input \"what is the value of b\" c ) (130 let d = ((b * b) - (4 * (a * c))))) )"

--test2 = "(130 let d = ((b*b) - (4.0 * (a * c))) )" 





{-
lookupEnv :: String -> State Global Sexpr
lookupEnv var = do
  (envState, penState) <- get
  case lookup var envState of
    Just x -> return x
    Nothing -> return undefined
-}




{-
toList :: Sexpr -> [(Sexpr, Sexpr)]
toList Nil = []
toList (Cons x y) = (x, Void) : toList y
-}





found' :: Sexpr -> Local -> State Global Graphic

--found' a b = return(Straight 0)



found' (Cons (Symbol "forward") (Cons (Number y) Nil)) local = do
  (envState, penState) <- get
  case penState of 
    True -> return $ (Straight (toGLFloat y))
    False -> return $ (Invisible (toGLFloat y))
found' (Cons (Symbol "forward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) local = do
  let z = (toGLFloat ((op2 oper) x y))
  (envState, penState) <- get
  case penState of 
    True -> return $ (Straight z)
    False -> return $ (Invisible z)

found' (Cons (Symbol "forward") (Cons (Symbol oper) Nil)) local = do
  case lookupLocal oper local of
    (Just x) -> do
              (envState, penState) <- get
              case penState of 
                True -> return $ (Straight (toGLFloat (unwrap x)))
                False -> return $ (Invisible (toGLFloat (unwrap x)))
    Nothing -> return (Err "Not a symbol")

found' (Cons (Symbol "backward") (Cons (Number y) Nil)) local = do
  (envState, penState) <- get
  case penState of 
    True -> return $ (Straight (toGLFloat (-y)))
    False -> return $ (Invisible (toGLFloat (-y)))
found' (Cons (Symbol "backward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) local = do
  let z = (toGLFloat ((op2 oper) x y))
  (envState, penState) <- get
  case penState of 
    True -> return $ (Straight (-z))
    False -> return $ (Invisible (-z)) 

found' (Cons (Symbol "backward") (Cons (Symbol oper) Nil)) local = do
  case lookupLocal oper local of
    (Just x) -> do
              (envState, penState) <- get
              case penState of 
                True -> return $ (Straight (toGLFloat (-(unwrap x))))
                False -> return $ (Invisible (toGLFloat (-(unwrap x))))
    Nothing -> return (Err "Not a symbol")
     
found'  (Cons (Symbol "repeat") (Cons (Number y) x)) local  = do 
  ex <- found' x local
  return (repeat' y ex)
found' (Cons (Symbol "right") (Cons (Number y) Nil)) local = do {return (Bend (toGLFloat (-y)))}
found' (Cons (Symbol "right") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) local = do {return (Bend (toGLFloat (-((op2 oper) x y))))}
found' (Cons (Symbol "right") (Cons (Symbol oper) Nil)) local = do
  case lookupLocal oper local of
    (Just x) -> do
              (envState, penState) <- get
              case penState of 
                True -> return $ (Bend (toGLFloat (-(unwrap x))))
                False -> return $ (Bend (toGLFloat (-(unwrap x))))
    Nothing -> return (Err "Not a symbol")



found' (Cons (Symbol "left") (Cons (Number y) Nil)) local = do {return (Bend (toGLFloat (y)))}
found' (Cons (Symbol "left") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) local= do {return (Bend (toGLFloat ((op2 oper) x y)))}
found' (Cons (Symbol "left") (Cons (Symbol oper) Nil)) local = do
  case lookupLocal oper local of
    (Just x) -> do
              (envState, penState) <- get
              case penState of 
                True -> return $ (Bend (toGLFloat ((unwrap x))))
                False -> return $ (Bend (toGLFloat ((unwrap x))))
    Nothing -> return (Err "Not a symbol")


found' (Cons (Symbol "make") (Cons (Symbol arg) (Cons expr Nil))) local = do
  let newValue = evalProc expr local

  return(Err ("Make Found" ++ (show newValue)))

found' (Cons (Symbol "penup") Nil) local = do {(envState, _) <- get; put (envState, True); return (Invisible 0)}
found' (Cons (Symbol "pendown") Nil) local = do {(envState, _) <- get; put (envState, False); return (Straight 0)}
found' (Cons x Nil) local = found' x local
found' (Cons (Symbol fname) args) local = do
  alpha <- lookupEnv fname []
  let args' = (toList args)
  case alpha of 
    Closure params body local' -> do 
                                  let l = zip params args'
                                  let local' = l:local'
                                  (envState, _) <- get 
                                  found' body local' 
          
    _ -> return (Err (fname))
found' (Cons x y) local = Join <$> found' x local <*> found' y local





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
  (envState, penState) <- get
  case penState of 
    True -> return $ (Straight (toGLFloat y))
    False -> return $ (Invisible (toGLFloat y))




found (Cons (Symbol "backward") (Cons (Number y) Nil)) = do
  (envState, penState) <- get
  case penState of 
    True -> return $ (Straight (toGLFloat (-y)))
    False -> return $ (Invisible (toGLFloat (-y)))
found (Cons (Symbol "backward") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do
  let z = (toGLFloat ((op2 oper) x y))
  (envState, penState) <- get
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
  (envState, penState) <- get
  let binding = (name, Closure (toList' args) bod [])
  let newState = (binding : envState, penState)
  put newState
  return (Err "Placed To")
found (Cons (Symbol "penup") Nil) = do {(envState, _) <- get; put (envState, True); return (Invisible 0)}
found (Cons (Symbol "pendown") Nil) = do {(envState, _) <- get; put (envState, False); return (Straight 0)}
found (Cons x Nil) = found x
found (Cons (Symbol fname) args) = do
  alpha <- lookupEnv fname []
  let args' = (toList args)
  case alpha of 
    Closure params body local -> do 
                                  let l = zip params args'
                                  let local = l:local
                                  (envState, _) <- get 
                                  found' body local 
                                  --return(Err "Found Closure")
          
    _ -> return (Err (fname))
found (Cons x y) = Join <$> found x <*> found y
found _  = return(Paint blue $ Straight 10)






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




 --return (repeat' y ex)
  --new comm
--found' (Cons (Symbol "right") (Cons (Number y) Nil)) = do {return (Bend (toGLFloat (-y)))}
--found' (Cons (Symbol "right") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil)) = do {return (Bend (toGLFloat (-((op2 oper) x y))))}
--found' (Cons (Symbol "right") (Cons (Symbol oper) Nil))  = do
--  (envState, penState, local) <- get
--  case lookupLocal oper (head local) of
--    (Just x) -> do
--              case penState of 
--                True -> return $ (Bend (toGLFloat (-(unwrap x))))
--                False -> return $ (Bend (toGLFloat (-(unwrap x))))
--    Nothing -> return (Err "Not a symbol")





--found' (Cons (Symbol "left") (Cons (Number y) Nil)) = do {return (Bend (toGLFloat (y)))}
--found' (Cons (Symbol "left") (Cons (Cons (Symbol oper) (Cons (Number x) (Cons (Number y) Nil))) Nil))= do {return (Bend (toGLFloat ((op2 oper) x y)))}
--found' (Cons (Symbol "left") (Cons (Symbol oper) Nil)) = do
--  (envState, penState,local) <- get
--  case lookupLocal oper (head local) of
--    (Just x) -> do
--              case penState of 
--                True -> return $ (Bend (toGLFloat ((unwrap x))))
--                False -> return $ (Bend (toGLFloat ((unwrap x))))
--    Nothing -> return (Err "Not a symbol")





--instance Show Sexpr where 
--    show (Symbol x) = x 
--    show (Number x) =  show x 
--    show Nil = "()"
--    show (Cons x y) =  "(" ++ show x ++ showCdr y ++ ")"



--showCdr :: Sexpr -> String
--showCdr Nil = ""
--showCdr (Cons x Nil) = " " ++ show x
--showCdr (Cons x v@(Cons y z)) = " " ++ show x ++ showCdr v
--showCdr (Cons x y) = " " ++ show x ++ " . " ++ show y
--showCdr x = " . " ++ show x

--simpleParser = do{ symb "print"; return(Print)}  +++ do{symb "input"; return (Input)} +++ do{symb "let"; return (Let)}




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





---- Koch curve

--koch angle 0 = Straight 1
--koch angle n = scale 2 . kochStep angle $ koch angle (n-1)

--kochStep angle g = join [g, Bend (-angle), g, Bend (2*angle), g, Bend (-angle), g]

---- Gosper curve

--gosper 0 = Straight 1
--gosper n = gosperStep $ gosper (n-1)

--gosperStep g = join [Bend 15, g, Bend (-60), g', Bend (-120), g', Bend 60, g, Bend 120, g, g, Bend 60, g', Bend (-75)]
--  where g' = mirror $ reverse g

---- Sierpinski tree

--sierpinski 0 = Straight 1
--sierpinski n = scale 0.5 . sierpinskiStep $ sierpinski (n-1)

--sierpinskiStep g = Straight 2 `Join` fork [Bend (-120) `Join` g, Bend 0 `Join` g, Bend 120 `Join` g]

---- Monkey tree

--d = (sqrt 3) / 3

--monkey 0 = Straight 1
--monkey n = monkeyStep $ monkey (n-1)

--monkeyStep g = join [Bend (-60), mirror g, reverse g, Bend 60, g, Bend 60, reverse g, Bend 150, scale d . reverse $ g, scale d . mirror $ g, Bend (-60), scale d . mirror . reverse $ g, Bend (-60), scale d . mirror . reverse $ g, scale d g, Bend (-90), mirror . reverse $ g, g]

---- Logo

--starfish angle step = join . concat $ take 90 [[Straight 1, Bend angle] | angle <- [angle, angle + step .. ]]

--stars angle = repeat' 5 $ join . concat $ take 8 [[Straight i, Bend angle] | i <- [1..]]

--logo n x dx y dy = join . concat $ take n [[Straight (x + i*dx), Bend (y + i*dy)] | i <- [1..]]

--starfish' angle step = logo 90 1 0 angle step

--stars' angle = repeat' 5 $ logo 8 1 1 angle 0

---- Marroquin pattern

--row n = join [repeat' n $ polygon 4 `Join` Invisible 20, Bend 180, Invisible $ fromIntegral n * 20, Bend (-90), Invisible 20, Bend (-90)]

--grid n = join [Bend (-135), Invisible $ sqrt 2 * 10 * fromIntegral (n-1), Bend 135, repeat' n $ row n]

--marroquin n = fork [Bend 120 `Join` g, Bend 60 `Join` g, g] where g = grid n

---- Wow

--wow = scale 1.5 $ repeat' 71 $ (repeat' 360 $ Straight 1 `Join` Bend 1) `Join` Bend 5

--interaction





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


evalOutside1 value = value
