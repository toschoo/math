module Algebra
where

  import           Data.List (findIndex,intercalate,sort,delete,
                              nub,partition,intersperse)
  import qualified Data.List.Split as Sp
  import           Data.Char (isDigit)
  import           Data.Either (rights)
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

  exp2num :: Exp -> Either String Int
  exp2num (Exp (Pos (Num n))) = Right n
  exp2num (Exp (Neg (Num n))) = Right (-n)
  exp2num _                   = Left "NaN"

  isNum :: Exp -> Bool
  isNum (Exp (Pos (Num _))) = True
  isNum (Exp (Neg (Num _))) = True
  isNum _ = False

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
  eval (Mul es) = normalise $ mulE (map eval es)
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
                          {-
                          Exp v3 -> let rs = addM e1 [Mul [Exp v3]]
                                     in if length rs == 1 then rs++go ts
                                        else (head rs):go (tail rs ++ ts)
                          -}
                          _      -> t1:go (t2:ts)
              _      -> t1:go (t2:ts)

  ------------------------------------------------------------------------
  -- Add 2 multiplications
  ------------------------------------------------------------------------
  addM :: [Exp] -> [Exp] -> [Exp]
  addM m1 m2 = let e1 = mulE m1
                   e2 = mulE m2
                   r | e1 == e2  = [Mul [Exp (Pos $ Num 2), e1]]
                     | otherwise = -- map eval [e1,e2] 
                       let (ns1,os1) = partition isNum m1
                           (ns2,os2) = partition isNum m2
                           ns        = ns1++ns2
                           os        = os1++os2
                        in if null ns then map eval [e1,e2]
                           else if sort os1 /= sort os2 
                                then map eval [e1,e2]
                                else let is = map exp2num ns
                                         p  = sum $ rights is
                                         n  | p < 0 = Exp (Neg (Num (-p)))
                                            | otherwise = Exp (Pos (Num p))
                                      in if p == 0 then [n]
                                         else if p == 1 then os1
                                              else [Mul (n:os1)]
                in r

  ------------------------------------------------------------------------
  -- Apply multiplication
  ------------------------------------------------------------------------
  mulE :: [Exp] -> Exp
  mulE [] = Mul []
  mulE [x] = x
  mulE es  = let (ns,os) = partition isNum es
                 is      = map exp2num ns
                 p       = product $ rights is
                 n | p < 0     = Exp (Neg (Num (-p)))
                   | otherwise = Exp (Pos (Num   p))
              in if null ns then Mul os
                 else if p == 0 || null os then n
                      else if length (nub os) == 1 then Mul [n,head os]
                           else Mul (n:os)

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
  ------------------------------------------------------------------------
  factor :: Exp -> Exp -> Either String Exp
  factor e (Add as) = do
    rs <- factorFrom e as
    Right (Mul [e,Add rs])
  factor _ _        = Left "second expression is not a sum"

  factorFrom :: Exp -> [Exp] -> Either String [Exp]
  factorFrom e []  = Right []
  factorFrom e ((Mul x):xs) = let x' = delete e x -- here we need division!
                               in if x' == x then Left "cannot factor"
                                  else do
                                    xs' <- factorFrom e xs
                                    Right (Mul x':xs')
  factorFrom _ _ = Left "not a sum of products" -- here we need division

  ------------------------------------------------------------------------
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
  normalise (Add [e]) = normalise e
  normalise (Add es) = Add (go es)
    where go [] = []
          go (x:xs) = case x of
                        Add ts -> go ts ++ go xs
                        _      -> [normalise x] ++ go xs
  normalise (Mul [])  = Mul []
  normalise (Mul [e]) = normalise e
  normalise (Mul es)  = Mul (go es)
    where go [] = []
          go (x:xs) = case x of
                        Mul ts -> go ts ++ go xs
                        _      -> [normalise x] ++ go xs
  normalise es = es

  ------------------------------------------------------------------------
  -- preprocessing
  ------------------------------------------------------------------------
  data Brack = Brack   [String]
             | NoBrack [String]
    deriving (Show)

  prettyBrack :: Brack -> String
  prettyBrack (Brack s)   = '(' : concat s ++ ")"
  prettyBrack (NoBrack s) = concat s

  data Ops = Oplus [String]
           | Omin  [String]
           -- Odiv
    deriving (Show)

  ------------------------------------------------------------------------
  -- turn "ab(c+d)" into "(a*b)(c+d)"
  ------------------------------------------------------------------------
  preprocess :: String -> String -- must be recursive
  preprocess s = connect $ bracketS $ map prettyBrack $ concat [
                   splitP (prettyBrack x) | x <- mapMul (splitP s)]

  ------------------------------------------------------------------------
  -- Two strings are connected with each other by an operator
  ------------------------------------------------------------------------
  areConnected :: String -> String -> Bool
  areConnected s1 s2 | null s1 || null s2  = True
                     | head s2 `elem` ops  = True
                     | last s1 `elem` ops  = True
                     | otherwise           = False

  ------------------------------------------------------------------------
  -- Put unconnected strings into brackets
  ------------------------------------------------------------------------
  bracketS :: [String] -> [String]
  bracketS []  = []
  bracketS [x] = [x]
  bracketS (x:y:zs) | areConnected x y = x : bracketS (y:zs)
                    | otherwise        = let g = takeWhileNotCon (y:zs)
                                             e = last g
                                          in ('(' : x) : init g ++ [e++")"] ++
                                             bracketS (dropWhileNotCon (y:zs))

  dropWhileNotCon :: [String] -> [String]
  dropWhileNotCon []  = []
  dropWhileNotCon [x] = []
  dropWhileNotCon (x:y:zs) | areConnected x y = (y:zs)
                           | otherwise = dropWhileNotCon (y:zs)

  takeWhileNotCon :: [String] -> [String]
  takeWhileNotCon []  = []
  takeWhileNotCon [x] = [x]
  takeWhileNotCon (x:y:zs) | areConnected x y = [x]
                           | otherwise = x : takeWhileNotCon (y:zs) 

  ------------------------------------------------------------------------
  -- insMul on list of brackets
  ------------------------------------------------------------------------
  mapMul :: [Brack] -> [Brack]
  mapMul [] = []
  mapMul (Brack x:xs)   = Brack (map insMul x) : mapMul xs
  mapMul (NoBrack x:xs) = NoBrack (map insMul x) : mapMul xs

  ------------------------------------------------------------------------
  -- Split a string into bracketed segements
  ------------------------------------------------------------------------
  splitP :: String -> [Brack]
  splitP [] = []
  splitP (x:xs) | x == '(' = let s = takeWhile (/= ')') xs
                              in Brack [s] : 
                                 splitP (drop 1 $ dropWhile (/= ')') xs)
                | otherwise = let s = takeWhile (/= '(') xs
                               in NoBrack [x:s] :
                                  splitP (dropWhile (/= '(') xs)

  ------------------------------------------------------------------------
  -- Insert mul in "ab" or "12c"
  ------------------------------------------------------------------------
  insMul  :: String -> String
  insMul  = go ""
    where go t []  | length t > 1 = t2str t
                   | otherwise    = t
          go t (x:xs) | x `elem` ops = 
                        if null t
                          then x : go "" xs 
                          else if length t < 2 
                               then t ++ [x] ++ go "" xs
                               else t2str t ++ [x] ++ go "" xs
                      | otherwise = go (t++[x]) xs

  t2str :: String -> String
  t2str s = let xs = filter (not . null) (go s)
             in '(' : intercalate "*" xs ++ ")"
    where go [] = []
          go (x:xs) = let z = takeWhile isDigit xs
                          (n,y) | isDigit x = (x:z,[])
                                | otherwise = (z,[x])
                       in n:y:go (dropWhile isDigit xs)

  ------------------------------------------------------------------------
  -- Connect unconnected string with "*"
  ------------------------------------------------------------------------
  connect :: [String] -> String
  connect [] = []
  connect [x] = x
  connect [x,x2] | last x `elem` ops || head x2 `elem` ops = x++x2 
                 | otherwise = x ++ "*" ++ x2
  connect (x:y:zs) = connect $ (connect [x,y]):zs

  ------------------------------------------------------------------------
  -- Legal operators
  ------------------------------------------------------------------------
  ops :: String
  ops = "+-*/"

  ------------------------------------------------------------------------
  -- parse
  ------------------------------------------------------------------------
  parseExp :: String -> Either String Exp
  parseExp s = trace (preprocess s) $
               case P.parse expression "" (preprocess s) of
                 Left e  -> Left (show e)
                 Right e -> Right (normalise e)

  ------------------------------------------------------------------------
  -- parse expression
  ------------------------------------------------------------------------
  expression :: Parser Exp
  expression = try opExp <|> try qvaronly <|> bracketed

  ------------------------------------------------------------------------
  -- parse "a `op` b"
  ------------------------------------------------------------------------
  opExp :: Parser Exp
  opExp = do
    e1 <- subexp
    o  <- operation
    e2 <- try opExp <|> subexp
    case o of
      AddOp -> return (Add [e1,e2])
      MulOp -> return (Mul [e1,e2])
      SubOp -> undefined -- return (Sub [e,es])
      DivOp -> undefined -- return (Div [e,es])

  ------------------------------------------------------------------------
  -- parse a subexpression, i.e. either a bracketed expression or a qvar
  ------------------------------------------------------------------------
  subexp :: Parser Exp
  subexp = bracketed <|> qvar

  ------------------------------------------------------------------------
  -- parse a qualified variable and nothing else
  ------------------------------------------------------------------------
  qvaronly :: Parser Exp
  qvaronly = do
    v <- qvar 
    eof
    return v

  ------------------------------------------------------------------------
  -- parse a bracketed expression, i.e. "(...)"  
  ------------------------------------------------------------------------
  bracketed :: Parser Exp
  bracketed = between (char '(') (char ')') expression

  ------------------------------------------------------------------------
  -- parse a qualified variable ("-a" or "b")
  ------------------------------------------------------------------------
  qvar :: Parser Exp
  qvar = try signedVar <|> unsignedVar

  ------------------------------------------------------------------------
  -- parse a signed variable ("-a")
  ------------------------------------------------------------------------
  signedVar :: Parser Exp
  signedVar = do 
    char '-' 
    v <- var
    return $ Exp (Neg v)

  ------------------------------------------------------------------------
  -- parse a unsigned variable ("a")
  ------------------------------------------------------------------------
  unsignedVar :: Parser Exp
  unsignedVar = do
    v <- var
    return $ Exp (Pos v)

  ------------------------------------------------------------------------
  -- parse a variable, either numeric or literal
  ------------------------------------------------------------------------
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

  ------------------------------------------------------------------------
  -- parse an operation
  ------------------------------------------------------------------------
  operation :: Parser Op
  operation =   (char '+' >> return AddOp)
            <|> (char '*' >> return MulOp)
            <|> (char '-' >> return SubOp)
            <|> (char '/' >> return DivOp)

  data Op = AddOp | MulOp | SubOp | DivOp

  ------------------------------------------------------------------------
  -- test
  ------------------------------------------------------------------------
  test :: String -> (Exp -> Exp) -> Either String String
  test s f = pretty . f <$> parseExp s

  tstParse :: String -> Either String String
  tstParse s = pretty <$> parseExp s

  tstEval :: String -> Either String String
  tstEval a = pretty <$> eval <$> parseExp a

  tstDist :: String -> String -> Either String String
  tstDist a b = do
    x1 <- parseExp a
    x2 <- parseExp b
    pretty <$> dist x1 x2

  tstFactor :: String -> String -> Either String String
  tstFactor a b = do
    e  <- parseExp a
    xs <- parseExp b
    pretty <$> normalise <$> factor e xs

