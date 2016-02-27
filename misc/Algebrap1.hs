module Algebrap1
where

  import           Data.List (findIndex,intercalate,sort)
  import           Data.Char (isDigit)
  import           Control.Monad.Trans.Either
  import           Control.Applicative ((<$>))
  import           Text.Parsec hiding (parse)
  import qualified Text.Parsec as P (parse)
  import           Text.Parsec.String

  import Debug.Trace (trace)

  ------------------------------------------------------------------------
  -- Raw Variable
  ------------------------------------------------------------------------
  data Var = Var Char
           | Num Int
    deriving (Eq,Ord,Show)

  mkVar :: String -> Either String Var
  mkVar x | numerical x   = Right (Num $ read x)
          | length x == 1 = Right (Var (head x))
          | otherwise     = Left ("not a var: " ++ x)

  numerical :: String -> Bool
  numerical = all isDigit

  prettyV :: Var -> String
  prettyV (Var c) = [c]
  prettyV (Num n) = show n

  ------------------------------------------------------------------------
  -- Qualified Variable
  ------------------------------------------------------------------------
  data Qual a = Neg a | Pos a
    deriving (Eq,Ord,Show)

  type QVar = Qual Var

  prettyQ :: QVar -> String
  prettyQ (Pos v) = prettyV v
  prettyQ (Neg v) = "-" ++ prettyV v

  ------------------------------------------------------------------------
  -- Expression
  ------------------------------------------------------------------------
  data Exp = Exp QVar
           | Add [Exp]
           | Mul [Exp]
           -- Div,Root
    deriving (Eq,Ord,Show)

  pretty :: Exp -> String
  pretty (Exp v)  = prettyQ v
  pretty (Add as) = intercalate "+" (map pretty as)
  pretty (Mul as) = bracket as

  bracket :: [Exp] -> String
  bracket [] = ""
  bracket ((Add x):es) = '(':pretty (Add x) ++ ")" ++ bracket es 
  bracket (e:es) = pretty e ++ bracket es

  ------------------------------------------------------------------------
  -- Addition
  ------------------------------------------------------------------------
  addV :: QVar -> QVar -> Exp
  addV (Pos (Var v1)) (Pos (Var v2)) | v1 == v2  = Mul [Exp (Pos (Num 2)), 
                                                        Exp (Pos (Var v1))]
                                     | otherwise = Add [Exp (Pos (Var v1)),
                                                        Exp (Pos (Var v2))]
  addV (Neg (Var v1)) (Neg (Var v2)) | v1 == v2  = Mul [Exp (Neg (Num 2)),
                                                        Exp (Pos (Var v1))]
                                     | otherwise = Add [Exp (Neg (Var v1)),
                                                        Exp (Neg (Var v2))]
  addV (Pos (Var v1)) (Neg (Var v2)) | v1 == v2  = Exp (Pos (Num 0))
                                     | otherwise = Add [Exp (Pos (Var v1)), 
                                                        Exp (Neg (Var v2))]
  addV (Neg (Var v1)) (Pos (Var v2)) = addV (Pos (Var v2)) (Neg (Var v1))

  addV (Pos (Num n1)) (Pos (Num n2)) = Exp (Pos (Num (n1+n2)))
  addV (Neg (Num n1)) (Neg (Num n2)) = Exp (Neg (Num (n1+n2)))
  addV (Pos (Num n1)) (Neg (Num n2)) | n2 > n1 = Exp (Neg (Num (n2-n1)))
                                     | otherwise = Exp (Pos (Num (n1-n2)))
  addV (Neg (Num n1)) (Pos (Num n2)) = addV (Pos (Num n2)) (Neg (Num n1))

  addV q1 q2 = Add [Exp q1, Exp q2]

  ------------------------------------------------------------------------
  -- Multiplication
  ------------------------------------------------------------------------
  mulV :: QVar -> QVar -> Exp
  mulV (Pos (Var v1)) (Pos (Var v2)) = Mul [Exp (Pos (Var v1)),
                                            Exp (Pos (Var v2))]
  mulV (Neg (Var v1)) (Neg (Var v2)) = Mul [Exp (Neg (Var v1)),
                                            Exp (Neg (Var v2))]
  mulV (Pos (Var v1)) (Neg (Var v2)) = Mul [Exp (Pos (Var v1)),
                                            Exp (Neg (Var v2))]
  mulV (Neg (Var v1)) (Pos (Var v2)) = Mul [Exp (Neg (Var v1)),
                                            Exp (Pos (Var v2))]

  mulV (Pos (Num n1)) (Pos (Num n2)) = Exp (Pos (Num (n1*n2)))
  mulV (Neg (Num n1)) (Neg (Num n2)) = Exp (Pos (Num (n1*n2)))
  mulV (Neg (Num n1)) (Pos (Num n2)) = Exp (Neg (Num (n1*n2)))
  mulV (Pos (Num n1)) (Neg (Num n2)) = Exp (Neg (Num (n1*n2)))

  mulV q1 q2 = Mul [Exp q1,Exp q2]

  ------------------------------------------------------------------------
  -- Eval (apply addition and multiplication)
  ------------------------------------------------------------------------
  eval :: Exp -> Exp
  eval (Add es) = normalise $ addE (map eval es)
  eval (Mul es) = mulE (map eval es)
  eval (Exp v)  = Exp v

  ------------------------------------------------------------------------
  -- Apply addition 
  ------------------------------------------------------------------------
  addE :: [Exp] -> Exp
  addE es = Add (go es)
    where go [] = []
          go [e] = [e]
          go (t1:t2:ts) = 
            case t1 of
              Exp v1 -> case t2 of
                          Exp v2 -> (addV v1 v2):go ts
                          _      ->  t1:go (t2:ts)
              Mul e1 -> case t2 of
                          Mul e2 -> let rs = addM e1 e2 
                                     in if length rs == 1 
                                          then rs++go ts 
                                          else (head rs):go (tail rs ++ ts)
                          _      -> t1:go (t2:ts)
              _      -> t1:go (t2:ts)

  ------------------------------------------------------------------------
  -- Add 2 multiplications
  ------------------------------------------------------------------------
  addM :: [Exp] -> [Exp] -> [Exp]
  addM m1 m2 = let e1 = mulE m1
                   e2 = mulE m2
                   r | e1 == e2  = [Mul [Exp (Pos $ Num 2), e1]]
                     | otherwise = [e1,e2]
                in r

  ------------------------------------------------------------------------
  -- Apply multiplication
  ------------------------------------------------------------------------
  mulE :: [Exp] -> Exp
  mulE es | allVars es = Mul (sort es)
          | otherwise  = Mul es

  ------------------------------------------------------------------------
  -- All expressions are vars
  ------------------------------------------------------------------------
  allVars :: [Exp] -> Bool
  allVars []         = True
  allVars (Exp v:es) = allVars es
  allVars _          = False

  ------------------------------------------------------------------------
  -- Distribute (multiplication over addition)
  ------------------------------------------------------------------------
  dist :: Exp -> Exp -> Either String Exp
  dist e (Add as) = Right (Add $ distOver e as)
  dist a b = Left "second expression is not a sum"

  distOver :: Exp -> [Exp] -> [Exp]
  distOver e []     = [e]
  distOver e [x]    = [Mul [e,x]]
  distOver e (x:xs) = (Mul [e,x]):distOver e xs

  ------------------------------------------------------------------------
  -- Factor (redo destribution)
  -- Order
  -- Permute
  ------------------------------------------------------------------------

  ------------------------------------------------------------------------
  -- On Strings
  ------------------------------------------------------------------------
  noop :: String -> Either String String
  noop s = pretty <$> parseExp s

  ------------------------------------------------------------------------
  -- normalise 
  ------------------------------------------------------------------------
  normalise :: Exp -> Exp
  normalise (Add [])  = Add []
  normalise (Add [e]) = e
  normalise (Add es) = Add (go es)
    where go [] = []
          go (x:xs) = case x of
                        Add ts -> go ts ++ go xs
                        _      -> (x:go xs)
  normalise (Mul [])  = Mul []
  normalise (Mul [e]) = e
  normalise es = es

  ------------------------------------------------------------------------
  -- parse
  ------------------------------------------------------------------------
  parseExp :: String -> Either String Exp
  parseExp s = case P.parse expression "" s of
                 Left e  -> Left (show e)
                 Right e -> Right (normalise e)

  expression :: Parser Exp
  expression = try multiExp <|> try opExp <|> subexp

  multiExp :: Parser Exp
  multiExp = do
    ps <- many1 bracketed
    if length ps == 1 then return (head ps)
                      else return (Mul ps)

  opExp :: Parser Exp
  opExp = do
    e  <- subexp
    o  <- operation
    es <- expression
    case o of
      AddOp -> return (Add [e,es])
      SubOp -> undefined -- return (Sub [e,es])
      DivOp -> undefined -- return (Div [e,es])

  subexp :: Parser Exp
  subexp = bracketed <|> parseMul

  bracketed :: Parser Exp
  bracketed = between (char '(') (char ')') expression

  parseMul :: Parser Exp
  parseMul = do
    vs <- many1 qvar
    if (length vs == 1) then return (head vs)
                        else return (Mul vs)

  qvar :: Parser Exp
  qvar = try signedVar <|> unsignedVar

  signedVar :: Parser Exp
  signedVar = do 
    char '-' 
    v <- var
    return $ Exp (Neg v)

  unsignedVar :: Parser Exp
  unsignedVar = do
    v <- var
    return $ Exp (Pos v)

  var :: Parser Var
  var = numVar <|> litVar

  numVar :: Parser Var
  numVar = do
    ns <- go []
    if null ns then fail "NaN"
               else return (Num (read ns))
    where go ns = do
            c <- lookAhead anyChar
            if isDigit c
              then char c >> go (c:ns)
              else return (reverse ns)

  litVar :: Parser Var
  litVar = do
    c <- oneOf ['a'..'z']
    return (Var c)

  operation :: Parser Op
  operation =   (char '+' >> return AddOp)
            <|> (char '-' >> return SubOp)
            <|> (char '/' >> return DivOp)

  data Op = AddOp | SubOp | DivOp

  whitespace :: Parser ()
  whitespace = spaces

  ------------------------------------------------------------------------
  -- test
  ------------------------------------------------------------------------
  test :: String -> (Exp -> Exp) -> Either String String
  test s f = pretty . f <$> parseExp s

  tstDist :: String -> String -> Either String String
  tstDist a b = do
    x1 <- parseExp a
    x2 <- parseExp b
    pretty <$> dist x1 x2

