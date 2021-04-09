import Control.Applicative.Lift

applyE :: Monoid e => Either e (a -> b) -> Either e a -> Either e b
applyE f x =
  case (f, x) of
    (Left e, Right _) ->
      Left e
    (Right _, Left e) ->
      Left e
    (Left e1, Left e2) ->
      Left (e1 <> e2)
    (Right g, Right y) ->
      Right (g y)