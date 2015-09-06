module Group
where

  class Group a where
    op    :: a -> a -> a
    idty  :: a
    inv   :: a -> a
    -- gen   :: a -> Bool
    group :: a -> [a]

  ------------------------------------------------------------------------
  -- Group example
  ------------------------------------------------------------------------
  data G = I | A | B | M
    deriving (Eq,Show)

  mul :: G -> G -> G
  mul I I = I
  mul I A = A
  mul I B = B
  mul I M = M
  mul A A = M
  mul A B = I
  mul A M = B
  mul B B = M
  mul B M = A
  mul M M = I
  mul x y = mul y x

  instance (Group) G where
    op   = mul
    idty = I
    inv I = I
    inv A = B
    inv B = A
    inv M = M
    group A = [A,M,B,I]
    group B = [B,M,A,I]
    group I = [I]
    group M = [M,I]

  testA :: G -> [G]
  testA I = [mul A I]
  testA x = let r = mul A x in r : testA r

