module Lindenmayer.System where

  import Lindenmayer.Common 

  -----------------------------------------------------------------------------
  -- Lindenmayer substitutes each 'x' found in a string omega
  --             by "y" for the rule 'x' -> "y"
  -----------------------------------------------------------------------------
  lindenmayer :: Omega -> [Rule] -> Int -> [String]
  lindenmayer [] _ _ = []
  lindenmayer o  _ 0 = [o]
  lindenmayer o r n = let s = substitute o r 
                       in  o : lindenmayer s r (n - 1)

  -----------------------------------------------------------------------------
  -- Build a string by substituting chars for strings
  -----------------------------------------------------------------------------
  substitute :: Omega -> [Rule] -> String
  substitute o r = concatMap (`rule` r) o

  -----------------------------------------------------------------------------
  -- if 'c' found in 'c' -> "x" return "x"
  -----------------------------------------------------------------------------
  rule :: Char -> [Rule] -> String
  rule c = concatMap apply 
    where apply r | c == fst r = snd r
                  | otherwise  = [c]

  -----------------------------------------------------------------------------
  -- Verifications:
  --   - each x in omega must be in voc or constant
  --   - each x in ('x' -> "y") must be in voc
  --   - each x in ('y' -> ['x']) must be in voc or constant
  --   - no redundant rules
  -----------------------------------------------------------------------------
  verifyOmega :: Omega -> Vocabulary -> Constant -> (Bool, String)
  verifyOmega [] _ _ = (True, "")
  verifyOmega (c:cs) v n | not (inVoc c v) &&
                           not (inVoc c n) = (False, 
                                              "Start Symbol invalid: " ++ [c])
                         | otherwise = verifyOmega cs v n

  verifyRules :: [Rule] -> Vocabulary -> Constant -> (Bool, String)
  verifyRules [] _ _ = (True, [])
  verifyRules (r:rs) v c = let (b, s) = verifyRule v c r 
                            in if not b then (b, s) 
                                 else verifyRules rs v c 

  verifyRule :: Vocabulary -> Constant -> Rule -> (Bool, String)
  verifyRule v n (h,b) | not (inVoc h v) &&
                         not (inVoc h n) = (False, 
                                       "Rule Head invalid: " ++ [h])
                       | otherwise = verifyRuleBody v n b

  verifyRuleBody :: Vocabulary -> Constant -> String -> (Bool, String)
  verifyRuleBody _ _ [] = (True, [])
  verifyRuleBody v n (c:cs) | not (inVoc c v) &&
                              not (inVoc c n) = (False,
                                     "Rule Body invalid: " ++ [c])
                            | otherwise = verifyRuleBody v n cs

  -----------------------------------------------------------------------------
  -- uniqueRules
  -----------------------------------------------------------------------------
  uniqueRules :: [Rule] -> (Bool, String)
  uniqueRules [] = (True, [])
  uniqueRules (r:rs) = 
    case lookup (fst r) rs of
      Nothing -> uniqueRules rs
      Just _  -> (False, "Redundant: " ++ [fst r])

  -----------------------------------------------------------------------------
  -- inVoc is used both for vocabulary and constants
  -----------------------------------------------------------------------------
  inVoc :: Char -> String -> Bool
  inVoc = elem

  -----------------------------------------------------------------------------
  -- complete verification
  -----------------------------------------------------------------------------
  verifySystem :: (Vocabulary, Constant, Omega, [Rule]) -> Bool
  verifySystem (v, c, o, r) = 
    let vo = verifyOmega o v c
        vr = verifyRules r v c 
        ur = uniqueRules r
     in eval [vo, vr, ur] 

  eval :: [(Bool, String)] -> Bool
  eval []            = True
  eval ((True,_):xs) = eval xs 
  eval ((False,s):_) = error s

