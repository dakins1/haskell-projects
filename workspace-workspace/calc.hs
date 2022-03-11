str = "7 + 232 - 3 * 4"
wordedStr = words str

data Op = Equal | Blus | Plus | Mult | Div | Minus deriving Show
data Token = ValueT Int | OperT Op deriving Show

tokenStr = [ValueT 7, OperT Plus, ValueT 232, OperT Minus, ValueT 3, OperT Mult, ValueT 4]

lexer :: String -> Maybe Token
lexer "+" = Just $ OperT Plus
lexer "-" = Just $ OperT Minus
lexer "*" = Just $ OperT Mult
lexer "/" = Just $ OperT Div
lexer "=" = Just $ OperT Equal
lexer "~" = Just $ OperT Blus
lexer num = if (and [x `elem` ['0'..'9'] | x <- num])
            then Just $ ValueT (read num)
            else Nothing

            
data ParseTree = ValueNode Int
               | OperNode Op ParseTree ParseTree
               deriving Show

parsedStr = OperNode Minus (OperNode Plus (ValueNode 7) 
                                          (ValueNode 2)
                           ) 
                           (OperNode Mult (ValueNode 3)
                                          (ValueNode 4)
                           )

unlex :: [Token] -> String
unlex toks = unwords $ map showTok toks
  where showTok :: Token -> String
        showTok (OperT op) = case op of 
                                Plus -> "+"
                                Minus -> "-"
                                Mult -> "*"
                                Div -> "/"
        showTok (ValueT x) = show x


--unlex toks makes a string str such that map dangerlex (words str) would return toks

--pprint :: ParseTree -> String

eval :: ParseTree -> Maybe Int
eval (ValueNode x) = Just x
{-eval (OperNode op l r) = do lv <- eval l
                            rv <- eval r
                            let ov = evalOp op
                            lv `ov` rv
 eval (OperNode op l r) = let lv = fromJust $ eval l
                             rv = fromJust $ eval r
                             ov = evalOp op
                         in lv `ov` rv
                         -}
eval (OperNode op l r) = case (eval l, eval r) of
                              (Just lv, Just rv) -> let ov = evalOp op
                                                    in lv `ov` rv
                              _ -> Nothing
evalOp :: Op -> Int -> Int -> Maybe Int
evalOp Minus x y = Just $ x - y
evalOp Mult x y = Just $ x * y
evalOp Div x y = Just $ x `div` y
evalOp Equal x y = Just $ if x == y then 1 else 0
evalOp Blus x 1 = Just $ x + 1
evalOp Blus x 0 = Just x
evalOp Blus x _ = Nothing 

parse :: [Token] -> ParseTree
parse [ValueT x] = ValueNode x
parse ((ValueT x):(OperT op):toks) = OperNode op (ValueNode x) (parse toks)
parse _ = error "AAAAH"


foo = let x = 10
          y = 20
          z = 30
       in x + y + z

weirdFun :: [Int] -> [Int] -> [Int]
weirdFun x y = 
  do xv <- x
     yv <- y
     return (2*xv+3*yv )
