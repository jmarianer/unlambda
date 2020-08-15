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

main = do
  putStr $ snd $ runWriter $ eval $ Apply CallNewline (Apply CallStar CallK)
