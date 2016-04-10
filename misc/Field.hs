module Field
where

  data Ext2 a = Ext2 a

  add :: (Num a,Num x) => x -> (a,a) -> (a,a) -> (a,a)
  add _ (a,b) (c,d) = (a+c,b+d)

  mul :: (Num a,Num x) => x -> (a,a) -> (a,a) -> (a,a)
  mul x (a,b) (c,d) = (a*c+x*bd,a*d+b*c)

