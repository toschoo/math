import Types
import Multi

test = mul2 [One] [One,One]

test2 a b = map (\_ -> Zero) a ++ map (\_ -> Zero) b
