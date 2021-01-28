-- opP :: Parser Op
-- opP = f <$> (charP '+' <|> charP '-') where
--     f '+' = Plus
--     f '-' = Minus

-- numberP :: Parser Term
-- numberP = TermNumber . read <$> notNull (spanP isDigit)

-- termP :: Parser Term
-- termP = numberP <|> idP 

-- exprExprP :: Parser Expr
-- exprExprP = ExprExpr <$> exprP <*> (ws *> opP <* ws) <*> termP

-- exprTermP :: Parser Expr
-- exprTermP = ExprTerm <$> termP 

-- exprP :: Parser Expr
-- exprP = exprTermP <|> exprExprP

-- goalP :: Parser Goal
-- goalP = exprP

-- idP :: Parser Term
-- idP = TermID <$> notNull (spanP isAlphaNum)