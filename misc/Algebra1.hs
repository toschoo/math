module Algebra1
where

  import Data.List (findIndex,intercalate,sort)
  import Data.Char (isDigit)
  import Control.Monad.Trans.Either

  import Debug.Trace (trace)

  data Var = Var Char
           | Num Int
    deriving (Eq,Ord,Show)

  mkVar :: String -> Either String Var
  mkVar x | numerical x   = Right (Num $ read x)
          | length x == 1 = Right (Var (head x))
          | otherwise     = Left ("not a var: " ++ x)

  prettyv :: Var -> String
  prettyv (Var c) = [c]
  prettyv (Num n) = show n

  addV :: Var -> Var -> Exp
  addV (Var v1) (Var v2) | v1 == v2  = Mul [show 2,[v1]]
                         | otherwise = Add [[v1],[v2]]
  addV (Num n1) (Num n2) = Exp (Num (n1+n2))
  addV (Var v)  (Num n)  = Add [[v], show n]
  addV (Num n)  (Var v)  = Add [show n,[v]]

  mulV :: Var -> Var -> Exp
  mulV (Var v1) (Var v2) = Mul [[v1,v2]]
  mulV (Num n1) (Num n2) = Exp (Num (n1*n2))
  mulV (Num n1) (Var v2) = Mul [show n1,[v2]]
  mulV (Var v1) (Num n2) = Mul [[v1],show n2]

  numerical :: String -> Bool
  numerical = all isDigit

  data Exp = Exp Var
           | Add [String]
           | Mul [String]
           -- Div,Root
    deriving (Eq,Ord,Show)

  eval :: String -> String
  eval s = case evalS s of
             Left e  -> error e
             Right x -> pretty x

  dist :: String -> String
  dist s = case evalDistS s of
             Left e  -> error e
             Right x -> pretty x

  evalS :: String -> Either String Exp
  evalS s = parse s >>= evalExp

  simplifyS :: String -> Either String Exp
  simplifyS s = parse s >>= simplify

  evalDistS :: String -> Either String Exp
  evalDistS s = parse s >>= distExp

  evalExp :: Exp -> Either String Exp
  evalExp (Exp v)  = Right (Exp v)
  evalExp (Add as) = addS as
  evalExp (Mul as) = mulS as

  distExp :: Exp -> Either String Exp
  distExp (Exp v)  = Right (Exp v)
  distExp (Add as) = addS as
  distExp (Mul as) = distS as

  addS :: [String] -> Either String Exp
  addS []  = Left "nothing to add"
  addS [x] = Left ("don't know how to add " ++ x)
  addS (a1:a2:as) = do
    x1 <- evalS a1
    x2 <- evalS a2
    s  <- add x1 x2
    if null as then Right s else go s as
    where go x [] = Right x
          go x1 (z:zs) = do
            x2 <- evalS z
            s  <- add x1 x2
            go s zs

  mulS :: [String] -> Either String Exp
  mulS []    = Left "nothing to mul"
  mulS [""]  = Left "nothing to mul"
  mulS [x]   = case mkVar x of
                 Left _  -> Right (Mul [x])
                 Right v -> Right (Exp v)
  mulS (a1:a2:as) = do
    x1 <- evalS a1
    x2 <- evalS a2
    p  <- mul x1 x2
    if null as then Right p else go p as
    where go x [] = Right x
          go x1 (z:zs) = do
            x2 <- evalS z 
            p  <- mul x1 x2
            go p zs

  distS :: [String] -> Either String Exp
  distS []   = Left "nothing to distribute"
  distS [""] = Left "nothing to distribute"
  distS [x]  = case mkVar x of
                 Left e  -> Right (Mul [x])
                 Right v -> Right (Exp v)
  distS (a1:a2:as) = do
     x1 <- evalS a1
     x2 <- evalS a2
     case x2 of
       Add bs -> do
         x3 <- go a1 bs
         if null as then Right (Mul $ (pretty x3):as) 
                    else Right (Mul $ ("(" ++ pretty x3 ++ ")"):as) 
       exp -> case x1 of
                Add bs -> distS (a2:a1:as)
                _      -> Right $ (Mul (a1:a2:as))
    where go a1 []  = Right (Mul [a1])
          go a1 [b] = do 
            p <- mulS [a1,b] 
            Right (Mul [pretty p])
          go a1 (b:bs) = do
            p  <- mulS [a1,b]
            ps <- go a1 bs
            Right (Add [pretty p,pretty ps])
                             
  add :: Exp -> Exp -> Either String Exp
  add (Exp v1) (Exp v2) = Right (addV v1 v2)
  add exp1 exp2 = do
    x1 <- evalExp exp1
    x2 <- evalExp exp2
    case x1 of 
      Exp v1 -> case x2 of
                  Exp v2  -> add x1 x2
                  x       -> Right (Add [pretty x1,pretty x2])
      Mul m1 -> case x2 of
                  Mul m2 -> if equal (Mul m1) (Mul m2)
                              then Right (Mul (show 2:m1))
                              else Right (Add [pretty x1,pretty x2])
                  _      -> Right (Add [pretty x1, pretty x2])
      _      -> Right (Add [pretty x1,pretty x2])

  mul :: Exp -> Exp -> Either String Exp
  mul (Exp v1) (Exp v2) = Right (mulV v1 v2)
  mul exp1 exp2 = do
    x1 <- evalExp exp1
    x2 <- evalExp exp2
    case x1 of 
      Exp v1 -> case x2 of
                  Exp v2 -> mul x1 x2
                  Add _  -> distS [pretty x1, "(" ++
                                   pretty x2 ++ ")"]
                  _      -> Right (Mul [pretty x1,pretty x2])
      Add _  -> case x2 of
                  Add _ -> distS [pretty x1, "(" ++
                                  pretty x2 ++ ")"]
                  _     -> distS ["(" ++ pretty x1 ++ ")",
                                         pretty x2]
      _      -> case x2 of 
                  Add _ -> distS [pretty x1, "(" ++
                                  pretty x2 ++ ")"]
                  _     -> Right (Mul [pretty x1,pretty x2])

  simplify :: Exp -> Either String Exp
  simplify (Add []) = Left "empty add"
  simplify (Add xs) = do
    xs' <- simplifyArgs xs
    Right (Add xs')

  simplifyArgs :: [String] -> Either String [String]
  simplifyArgs []  = Right []
  simplifyArgs [s] = Right [s]
  simplifyArgs (s1:s2:ss) = do
    x1 <- evalS s1
    x2 <- evalS s2
    if equal x1 x2 then simplifyArgs (pretty (Mul [show 2,s1]):ss)
                   else do r <- simplifyArgs (s2:ss)
                           Right (s1:r)

  equal :: Exp -> Exp -> Bool
  equal (Exp v1) (Exp v2) = v1 == v2
  equal (Add a1) (Add a2) = equalN a1 a2
  equal (Mul a1) (Mul a2) = let b1 = map sort a1 
                                b2 = map sort a2
                                c1 = sort b1
                                c2 = sort b2
                             in c1 == c2
  equal _ _ = False

  equalN :: [String] -> [String] -> Bool
  equalN xs ys = go (sort xs) (sort ys)
    where go [] [] = True
          go [] _  = False
          go _  [] = False
          go (a:as) (b:bs) = 
            case evalS a of 
              Left _ -> False
              Right x1 -> case evalS b of
                            Left _ -> False
                            Right x2 -> if equal x1 x2 then go as bs
                                                       else False
            


  pretty :: Exp -> String
  pretty (Exp v)  = prettyv v
  pretty (Add as) = intercalate "+" as
  pretty (Mul as) = concat as

  type Stack = [String]

  push :: Stack -> String -> Stack
  push s e = e:s

  pop :: Stack -> Either String (String,Stack)
  pop [] = Left "Stack is empty"
  pop s  = Right (head s, tail s)

  toList :: Stack -> [String]
  toList = reverse

  addStk :: Stack -> String -> Either String Stack
  addStk s y = do
    (x,s') <- pop s
    Right (push s' (x++"+"++y))

  mulStk :: Stack -> String -> Either String Stack
  mulStk s y = do
    (x,s') <- pop s
    Right (push s' (x++y))

  parse :: String -> Either String Exp
  parse s = getexp s "" []

  getexp :: String -> String -> [String] -> Either String Exp
  getexp "" "" []  = Left "nothing to parse"
  getexp "" "" s   = Right (Mul s)
  getexp "" t  s   = Right (Mul (s++[t]))
  getexp ('(':xs) t s = do
    x <- close ('(' : xs) 
    case x of
      ("",_) -> Left "empty parentheses"
      (a,"") -> if null s && null t 
                then let a' = takeWhile (/= ')') $ drop 1 a
                      in getexp a' "" []
                else if null t then Right (Mul (s++[a])) 
                               else Right (Mul (s++[t,a]))
      (a,zs) -> if null t then getexp zs "" (s++[a])
                          else if null s then getexp zs "" [t++a]
                                         else getexp zs "" (s++[t++a])
  getexp ('+':xs) t s = if null t then Right (Add (s++[xs]))
                                  else if null s then Right (Add [t,xs])
                                       else Right (Add (init s++[last s++t]++[xs]))
  getexp (x:xs)   t s = getexp xs (t++[x]) s

  close :: String -> Either String (String,String)
  close s = case findIndex (==')') s of
              Nothing -> Left "parentheses not closed"
              Just i  -> Right (take (i+1) s,drop (i+1) s)
