joinRules :: String -> [String]
joinRules s = 
    let (r, w) = oneRule s
    in if w /= [] then r:(joinRules w)
       else [r]

oneRule :: String -> (String, String)
oneRule [] = ([], [])
oneRule (c:cs) =
    if c == ')' then ([], cs)
      else let (r, w) = oneRule cs
           in if c == '(' then (r, w) else ((c:r), w)

