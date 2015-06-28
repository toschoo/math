
infix <|>
(<|>) :: Bool -> Bool -> Bool
p <|> q = not(not p && not q)
