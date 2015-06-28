module Lindenmayer.Config where

  import qualified Lindenmayer.Common as Com
  import qualified Text.ParserCombinators.Parsec as Parsec

  ---------------------------------------------------------
  -- Section [system] [model] and [graphics]
  ---------------------------------------------------------
  data Section = System   {sname :: String,
                           voc   :: Com.Vocabulary,
                           const :: Com.Constant,
                           om    :: Com.Omega,
                           rule  :: [Com.Rule]}
               | Model    {mname :: String}
               | Graphics {wSize  :: Com.Point,
                           startP :: Com.Point,
                           length :: Int,
                           angle  :: Int,
                           comp   :: Double}
    deriving (Show)

  ---------------------------------------------------------
  -- Config as order of three sections
  -- We could also use a triple or a list
  -- This way the access is simpler
  ---------------------------------------------------------
  data Config = Config {sys :: Section,
                        mod :: Section,
                        grf :: Section}
    deriving (Show)

  type Parameter = (String, String)

  --------------------------------------------------------------
  -- Creating a config is parsing a string
  --------------------------------------------------------------
  configure :: String -> Config
  configure s = case Parsec.parse parseConfig "Lindenmayer" s of
    Left  err -> error ("Config File is invalid: " ++ show err)
    Right c   -> c

  --------------------------------------------------------------
  -- We parse three sections and order them
  -- Note: If we had used a set
  --       the ugly ordering function 
  --       would not have been needed
  --------------------------------------------------------------
  parseConfig :: Parsec.Parser Config
  parseConfig = do
    s1 <- section
    s2 <- section
    s3 <- section
    let (s1', s2', s3') = orderSections s1 s2 s3
    return $ Config s1' s2' s3'

  --------------------------------------------------------------
  -- A section consists of a head [...] and a body
  --------------------------------------------------------------
  section :: Parsec.Parser Section
  section = do
    skipTrash
    h <- sectionHead
    sectionBody h

  --------------------------------------------------------------
  -- The head
  --------------------------------------------------------------
  sectionHead :: Parsec.Parser String
  sectionHead = do
    Parsec.char '['
    s <- Parsec.many1 (Parsec.noneOf symbols)
    Parsec.char ']'
    linebreak
    skipTrash
    return s

  --------------------------------------------------------------
  -- The Body is a collection of parameters
  -- which ones depend on the type of the section
  -- defined by the head
  --------------------------------------------------------------
  sectionBody :: String -> Parsec.Parser Section
  sectionBody "system"   = systemBody
  sectionBody "model"    = modelBody 
  sectionBody "graphics" = graphicsBody
  sectionBody s          = fail ("Unknown Section " ++ s)

  systemBody :: Parsec.Parser Section
  systemBody = do 
    p <- parameters
    let n = getPValue "name"  p
    let v = getPValue "voc"   p
    let c = getPValue "const" p
    let r = getPValue "rules" p
    let o = getPValue "omega" p
    if not $ check [n, v, c, r, o]
      then fail "Section [system] incomplete"
      else return $ System n v c o (s2Rule r)  

  modelBody :: Parsec.Parser Section
  modelBody = do
    p <- parameters
    let n = getPValue "name" p
    if null n 
      then fail "Section [model] incomplete"
      else return $ Model n

  graphicsBody :: Parsec.Parser Section
  graphicsBody = do
    p <- parameters
    let w = (toPoint . getPValue "wsize") p 
        s = (toPoint . getPValue "start") p  
        l = getPValue "length" p
        a = getPValue "angle" p
        c = getPValue "compress" p
    if areIntegers [l,a]
      then if isReal c
             then return $ Graphics w s (read l) 
                                        (read a) 
                                        (read c::Double)
             else fail ("compress is not a real: " ++ c)
      else fail ("Not an Integer: '" ++ 
                 findFaultyInteger [l,a] ++ "'")

  --------------------------------------------------------------
  -- Parameters
  --------------------------------------------------------------
  parameters :: Parsec.Parser [Parameter]
  parameters = Parsec.many1 parameter

  parameter :: Parsec.Parser Parameter
  parameter = do
    n <- Parsec.many1 (Parsec.noneOf symbols)
    Parsec.spaces
    Parsec.char '='
    Parsec.spaces
    v <- Parsec.many escChar
    linebreak
    skipTrash
    return (n, v)
  
  --------------------------------------------------------------
  -- Some helpers - some of them are perhaps worth
  -- of entering a common parsing helper library
  --------------------------------------------------------------
  comment :: Parsec.Parser () 
  comment = do
    Parsec.char '#'
    Parsec.skipMany (Parsec.noneOf "\n")
    Parsec.skipMany1 linebreak

  escChar :: Parsec.Parser Char
  escChar = do
    c <- Parsec.noneOf "\n"
    if c == '\\'
      then do
        c' <- Parsec.anyChar
        if c' == '\n' then return ' ' else return c'
      else return c

  skipTrash :: Parsec.Parser ()
  skipTrash = do
    Parsec.optional (Parsec.skipMany1 comment)
    Parsec.optional (Parsec.skipMany1 (Parsec.oneOf "\n "))
    Parsec.optional (Parsec.skipMany1 comment)
    return ()

  linebreak :: Parsec.Parser Char
  linebreak = Parsec.char '\n'

  stripWhitespace :: String -> String
  stripWhitespace = filter (' ' /=)

  symbols :: String
  symbols = " \n[]="

  --------------------------------------------------------------
  -- Rule Parsing 
  -- we first build a set of strings (joinRules)
  --    => each string representing a rule
  -- we then split the string by Token '>' (sSetRule)
  --    => using splitTok
  --------------------------------------------------------------
  s2Rule :: String -> [Com.Rule]
  s2Rule [] = []
  s2Rule s  = 
    let r = joinRules s
     in sSet2Rule r

  sSet2Rule :: [String] -> [Com.Rule]
  sSet2Rule [] = []
  sSet2Rule (s:ss) = let (h, b) = splitTok '>' s
                      in (head h, b) : sSet2Rule ss

  joinRules :: String -> [String]
  joinRules [] = []
  joinRules (c:cs) =
    if c /= '(' then joinRules cs -- ignore everthing between ) (
      else let (r, w) = oneRule cs -- get one Rule
           in if w /= [] then r:joinRules w -- join them together
              else [r]

  --------------------------------------------------------------
  -- we read everything between ( )
  --------------------------------------------------------------
  oneRule :: String -> (String, String)
  oneRule [] = ([], [])
  oneRule (c:cs) =
    if c == ')' then ([], cs) -- one rule found
      else 
        let (r, w) = oneRule cs -- get next char
            r'     = stripWhitespace r -- get rid of annoying whitespace
         in if c == '(' || c == ' ' then (r', w) else (c:r', w) -- ignore '('

  --------------------------------------------------------------
  -- Number Parsing
  -- we turn a string containing a ',' 
  -- into a tuple of two numbers using splitTok
  -- The second argument is a default
  -- Note: If the parse of one of the numbers fails
  --       we issue an error
  --------------------------------------------------------------
  toPoint :: String -> Com.Point 
  toPoint s  = case parseNum s1 of
                 Right i1 -> case parseNum s2 of
                               Right i2 -> (i1, i2)
                               Left e2  -> error (msg ++ e2)
                 Left e1  -> error (msg ++ e1)
               where (s1, s2) = splitTok ',' s
                     msg      = "Not a point: '" ++ s ++ "'" ++ " - "

  parseNum :: String -> Either String Int 
  parseNum s = if isInteger s then Right (read s)
                              else Left ("Not a number: '" ++ s ++ "'")
               where isInteger = all isDigit . stripWhitespace

  isDigit :: Char -> Bool
  isDigit = (`elem` ['0'..'9'])

  isReal :: String -> Bool
  isReal = all isDigitOrDot . stripWhitespace
           where isDigitOrDot c = isDigit c || c == '.'

  areIntegers :: [String] -> Bool
  areIntegers [] = True
  areIntegers (s:ss) | isInteger s = areIntegers ss
                     | otherwise   = False
                     where isInteger n = case parseNum n of 
                                         Left  _ -> False
                                         Right _ -> True

  findFaultyInteger :: [String] -> String
  findFaultyInteger [] = ""
  findFaultyInteger (s:ss) = case parseNum s of
                               Left e -> e
                               Right _ -> findFaultyInteger ss

  --------------------------------------------------------------
  -- find a tuple in a list of tuples
  -- where fst = string
  -- Note: Not a good fold, 
  --       in most cases we stop before the end of the list
  --------------------------------------------------------------
  getPValue :: String -> [Parameter] -> String 
  getPValue _ [] = ""
  getPValue s (p:ps) | s == fst p = snd p
                     | otherwise  = getPValue s ps

  --------------------------------------------------------------
  -- Candidate for a library 
  --------------------------------------------------------------
  splitTok :: Char -> String -> (String, String)
  splitTok _ [] = ([], [])
  splitTok delim s = if not (null t) then (h, tail t) else (h, t)
                     where (h, t) = break (delim ==) s

  --------------------------------------------------------------
  -- No empty strings allowed
  -- Note: Not a good fold, 
  --       in error case we stop before the end of the list
  --------------------------------------------------------------
  check :: [String] -> Bool
  check [] = True
  check (x:xs) | null x    = False
               | otherwise = check xs

  --------------------------------------------------------------
  -- This ugly function is hidden here at the end
  --------------------------------------------------------------
  orderSections :: Section -> Section -> Section -> (Section, Section, Section)
  orderSections s1@System  {} 
                s2@Model   {}
                s3@Graphics{} = (s1, s2, s3)
  orderSections s1@System  {} 
                s3@Graphics{} 
                s2@Model   {} = (s1, s2, s3)
  orderSections s2@Model   {} 
                s1@System  {} 
                s3@Graphics{} = (s1, s2, s3)
  orderSections s2@Model   {} 
                s3@Graphics{} 
                s1@System  {} = (s1, s2, s3)
  orderSections s3@Graphics{} 
                s2@Model   {}
                s1@System  {} = (s1, s2, s3)
  orderSections s3@Graphics{} 
                s1@System  {}
                s2@Model   {} = (s1, s2, s3)
  orderSections _ _ _ = error 
                        "Config needs a section for System, Model and Graphics"
