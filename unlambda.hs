data Value =
    K
  | K1 Value
  | S
  | S1 Value
  | S2 Value Value
  deriving Show

data Call =
    CallS
  | CallK
  | Apply Call Call
  deriving Show

eval :: Call -> Value
eval CallS = S
eval CallK = K
eval (Apply f1 f2) = apply (eval f1) (eval f2)

apply :: Value -> Value -> Value
apply K x = K1 x
apply (K1 x) y = x
apply S x = S1 x
apply (S1 x) y = S2 x y
apply (S2 x y) z = apply (apply x y) (apply y z)

main = do
  print $ eval $ Apply (Apply (Apply CallS CallK) CallS) CallS
