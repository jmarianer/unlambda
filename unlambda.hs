import Control.Monad.Writer

data Value =
    K
  | K1 Value
  | S
  | S1 Value
  | S2 Value Value
  | I
  | PrintChar Char
  deriving Show

data Call =
    CallS
  | CallK
  | CallI
  | Apply Call Call
  | CallStar
  | CallNewline
  deriving Show

eval :: Call -> Writer String Value
eval CallS = return S
eval CallK = return K
eval CallI = return I
eval (Apply f1 f2) = do
  v1 <- eval f1
  v2 <- eval f2
  apply v1 v2
eval CallStar = return $ PrintChar '*'
eval CallNewline = return $ PrintChar '\n'

apply :: Value -> Value -> Writer String Value
apply K x             = return $ K1 x
apply (K1 x) y        = return $ x
apply S x             = return $ S1 x
apply (S1 x) y        = return $ S2 x y
apply (S2 x y) z      = do
  i1 <- apply x z
  i2 <- apply y z
  apply i1 i2
apply I x             = return x
apply (PrintChar c) x = writer (x, [c])

parse :: String -> (Call, String)
parse (' ':rest) = parse rest
parse ('\n':rest) = parse rest
parse ('`':rest) =
  let (f1, rest1) = parse rest
      (f2, rest2) = parse rest1
  in (Apply f1 f2, rest2)
parse ('s':rest) = (CallS, rest)
parse ('k':rest) = (CallK, rest)
parse ('i':rest) = (CallI, rest)
parse ('r':rest) = (CallNewline, rest)
parse ('.':'*':rest) = (CallStar, rest)

main = do
  let prog = parse "\
\```s``s``sii`ki\
\  `k.*``s``s`ks\
\ ``s`k`s`ks``s``s`ks``s`k`s`kr``s`k`sikk\
\  `k``s`ksk "
  putStr $ snd $ runWriter $ eval $ fst prog
