module Group
where

  class Group a where
    op    :: a -> a -> a
    idty  :: a
    inv   :: a -> a
    -- gen   :: a -> Bool
    group :: a -> [a]

