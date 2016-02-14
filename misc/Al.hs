module Al
where
  
  import Data.List (sort, intersect, (\\))

  type Name = (Char,Int)

  showName :: Name -> String
  showName (c,0) = [c] 
  showName (c,s) = [c] ++ "_" ++ show s

  ------------------------------------------------------------------------
  -- Rational variable with roots
  ------------------------------------------------------------------------
  data Var = Var Int [Name]
           | Neg Var
           | Inv Var
           | P   Var Var
           | T   Var Var
           | Z   Int    -- ?
    deriving (Eq,Show)

  pretty :: Var -> String
  pretty (Var 1 n) = concatMap showName n
  pretty (Var f n) = show f ++ (concatMap showName n)
  pretty (Neg n)   = "-(" ++ show n ++ ")"
  pretty (Inv n)   = "1/" ++ show n
  pretty (P a (Neg n)) = pretty a ++ "-" ++ pretty n
  pretty (P a b)       = pretty a ++ "+" ++ pretty b
  pretty (T a (P b c)) = "(" ++ pretty a ++ ")(" ++ pretty (P b c) ++ ")"
  pretty (T a b)       = pretty a ++ pretty b
  
  eval :: Var -> Var
  eval (T (Z i) (Var ia as)) = Var (i*ia) as
  eval (T (Var ia as) (Z i)) = Var (i*ia) as
  eval (T (Z a) (Z b)) = Z (a*b)
  eval (P (Z a) (Z b)) = Z (a+b)
  eval (P (Var ia as) (Z i)) = Var (i+ia) as
  eval (P (Z i) (Var ia as)) = Var (i+ia) as
  eval (T (Var ia a) (Var ib b)) = Var (ia*ib) (a++b)
  eval (T (Var ic c) (P (Var ia a) (Var ib b))) = eval $ P (eval (T (Var ic c) (Var ia a)))
                                                           (eval (T (Var ic c) (Var ib b)))
  eval (T (P a b) (P c d)) = eval $ P (eval (T a (eval $ P c d)))
                                      (eval (T b (eval $ P c d)))
  eval (P (Var ia a) (Var ib b)) | sort a == sort b = Var (ia + ib) a
                                 | otherwise        = P (Var ia a) (Var ib b)
  eval (P (P a@(Var ia as) 
             b@(Var ib bs)) c@(Var ic cs)) | sort as == sort bs = eval (P (Var (ia+ib) as) c)
                                           | sort as == sort cs = eval (P (Var (ia+ic) as) b)
                                           | sort bs == sort cs = eval (P (Var (ib+ic) bs) c)
                                           | otherwise          = P (P a b) c
  eval (P (P a@(Var ia as) 
             b@(Var ib bs)) 
          (P c@(Var ic cs)
             d@(Var id ds))) | sort as == sort bs = eval (P (Var (ia+ib) as) (eval $ P c d))
                             | sort as == sort cs = eval (P (Var (ia+ic) as) (eval $ P b d))
                             | sort as == sort ds = eval (P (Var (ia+id) as) (eval $ P b c))
                             | sort bs == sort cs = eval (P (Var (ib+ic) bs) (eval $ P a d))
                             | sort bs == sort ds = eval (P (Var (ib+id) bs) (eval $ P a c))
                             | sort cs == sort ds = eval (P (Var (ic+id) cs) (eval $ P a b))
                             | otherwise          = P (P a b) c
  -- eval (P (P a b) c) = P (eval (P a b)) c
  -- eval (P c (P a b)) = P (eval (P a b)) c  
  eval (P a b) = case P (eval a) (eval b) of
                   P x@(Var _ _) y@(Var _ _) -> eval (P x y)
                   P x@(Z _)     y@(Z _)     -> eval (P x y)
                   x                         -> x
  eval x = x

  evalsum :: [Var] -> Var
  evalsum []  = Z 0
  evalsum [x] = x
  evalsum (x:y:zs) = evalsum ((eval $ P x y) : zs)


  factor :: Var -> Var
  factor (P (Var ia as) (Var ib bs)) = 
    case as `intersect` bs of
      [] -> P (Var ia as) (Var ib bs)
      cs -> T (Var 1 cs) (P (Var ia (as \\ cs)) (Var ib (bs \\ cs)))
  factor x = x

  test :: IO ()
  test = do
    let a = Var 1 [('a',0)]
    let b = Var 2 [('b',0)]
    let c = Var 1 [('c',0)]
    let d = eval $ T c (P a b)
    print d
    let e = factor d
    print e

  binom :: IO ()
  binom = do
    let a = Var 1 [('a',0)]
    let b = Var 1 [('b',0)]
    let c = P a b
    let d = eval $ T c c
    print d
    print $ pretty d
    let e = eval $ T c d
    print $ pretty e

  add :: IO ()
  add = do
    let a = Var 1 [('a',0),('b',0)]
    let b = Var 1 [('b',0),('a',0)]
    print $ pretty $ eval (P (P a b) (P a b))

   
