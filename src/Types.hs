module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Grammar

-- import qualified Text.PrettyPrint as PP

data Scheme = Scheme [String] Type deriving (Show)
type Subst = M.Map String Type
data Kind = Var | Fun deriving (Eq, Ord, Show)

class Types a where
  ftv :: a -> S.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TypeID n) = S.singleton n
  ftv (TypeBasic _)  = S.empty
  ftv (TypeArray t) = ftv t
  ftv (TypeFun t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TypeTuple t1 t2)  = ftv t1 `S.union` ftv t2
  ftv Void = S.empty

  apply s (TypeID n) = case M.lookup n s of
    Nothing -> TypeID n
    Just t -> t
  apply s (TypeBasic t) = TypeBasic t
  apply s (TypeArray t) = TypeArray (apply s t)
  apply s (TypeFun t1 t2) = TypeFun (apply s t1) (apply s t2)
  apply s (TypeTuple t1 t2) = TypeTuple (apply s t1) (apply s t2)
  apply s Void = Void

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `S.intersection` S.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv l = foldr (S.union . ftv) S.empty l

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (apply s1) s2 `M.union` s1

newtype TypeEnv = TypeEnv (M.Map (Kind, String) Scheme)

instance Show TypeEnv where
    show (TypeEnv env) = show env

remove :: TypeEnv -> Kind -> String -> TypeEnv
remove (TypeEnv env) k var = TypeEnv $ M.delete (k, var) env

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (M.elems env)
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = S.toList (ftv t `S.intersection` ftv env)

data TIEnv = TIEnv { }
data TIState = TIState { tiSupply :: Int, tiSubst :: Subst }

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = do
    (res,st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    return (res,st)
        where initTIEnv = TIEnv { }
              initTIState = TIState { tiSupply = 0, tiSubst = M.empty }

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    return (TypeID (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM newTyVar vars -- (λ → newTyVar "a")
    let s = M.fromList (zip vars nvars)
    return $ apply s t

mgu :: Type -> Type -> TI Subst
mgu (TypeFun l1 r1) (TypeFun l2 r2) = do
    s1 <- mgu l1 l2
    s2 <- mgu (apply s1 r1) (apply s1 r2)
    return $ s1 `composeSubst` s2
mgu (TypeArray t1) (TypeArray t2) = mgu t1 t2
mgu (TypeTuple l1 r1) (TypeTuple l2 r2) = do
    s1 <- mgu l1 l2
    s2 <- mgu (apply s1 r1) (apply s1 r2)
    return $ s1 `composeSubst` s2
mgu (TypeID u) t = varBind u t
mgu t (TypeID u) = varBind u t
mgu (TypeBasic t1) (TypeBasic t2)
    | t1 == t2 = return nullSubst
    | otherwise = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t
    | TypeID u == t = return nullSubst
    | u `S.member` ftv t = throwError $ "occur check fails: " ++ u ++ " vs. " ++ show t
    | otherwise = return (M.singleton u t)

tiDecl :: TypeEnv -> Decl -> TI (Subst, Type)
-- tiDecl e (DeclVarDecl v) = tiVarDecl e v
tiDecl e (DeclFunDecl f) = tiFunDecl e f

--tiVarDecl :: TypeEnv -> VarDecl -> TI (Subst, Type)
--tiVarDecl env (VarDeclVar s e) = do
--    tv <- newTyVar "a"
--    let TypeEnv env' = removeVar env s
--    let env'' = TypeEnv (env' `M.union` M.singleton s (Scheme [] tv))
--    (s1, t1) <- tiExp env'' e
--    return (s1 , TypeFun (FunType [apply s1 tv] (RetTypeType t1)))
-- tiVarDecl (TypeEnv env) (VarDeclVar s e) = case M.lookup s env of
--     Nothing -> throwError $ "unbound variable: " ++ s
--     Just sigma -> do
--         t <- instantiate sigma
--         return (nullSubst, t)
--tiVarDecl e (VarDeclType t _ _) = return (nullSubst, t)

-- SPL -> TypeEnv
-- fun x() { return y() && True; }
-- fun y() { return 3; }
-- { y :: -> Bool, -> c }
-- y :: (ai -> ai+1 -> ...)
-- x(a, b) :: Int Int -> Bool { return f() && b; }

tiFunDecl :: TypeEnv -> FunDecl -> TI (Subst, Type)
tiFunDecl env (FunDecl _ (args) (Just t) _ _) = return (nullSubst, t)
-- FunDecl zonder type

tiStmt :: TypeEnv -> Stmt -> TI (Subst, Type)
tiStmt = undefined
-- Checken dat exp in if enzo bool is

tiFunCall :: TypeEnv -> FunCall -> TI (Subst, Type)
tiFunCall = undefined

-- We moeten iets aan type toevoegen/ veranderen/ nieuw data type toevoegen
-- dat ervoor zorgt dat we a -> b kunnen doen (functie types)

-- Dit misschien?
tiOp2 :: Op2 -> (Type, Type, Type)
tiOp2 Plus = (TypeBasic IntType, TypeBasic IntType, TypeBasic IntType)

tiExp :: TypeEnv -> Exp -> TI (Subst, Type)
tiExp env (Exp o e1 e2) = do
    let (t1, t2, t3) = tiOp2 o
    -- Checken of e1 type t1 heeft en e2 type t2
    return (nullSubst, t3)
tiExp env (ExpOp1 o e) = tiExp env e -- iets doen met het type van o
tiExp env (ExpTuple (e1, e2)) = do
    (s1, t1) <- tiExp env e1
    (s2, t2) <- tiExp env e1 -- s1 applyen?
    return (s1 `composeSubst` s2, TypeTuple t1 t2)
tiExp env (ExpBrackets e) = tiExp env e
tiExp env (ExpField s fs) = undefined -- checken of s in env zit ofzo
tiExp _ (ExpInt _) = return (nullSubst, TypeBasic IntType)
tiExp _ (ExpBool _) = return (nullSubst, TypeBasic BoolType)
tiExp _ (ExpChar _) = return (nullSubst, TypeBasic CharType)
tiExp env (ExpFunCall f) = tiFunCall env f
tiExp _ ExpEmptyList = return (nullSubst, TypeArray $ TypeBasic CharType)

typeInference :: M.Map (Kind, String) Scheme -> Exp -> TI Type
typeInference env e = do
    (s,t) <- tiExp (TypeEnv env) e
    return (apply s t)

test :: Exp -> IO ()
test e = do
    (res, _) <- runTI (typeInference M.empty e)
    case res of
        Left err -> putStrLn $ "error: " ++ err
        Right t -> putStrLn $ show e ++ " :: " ++ show t