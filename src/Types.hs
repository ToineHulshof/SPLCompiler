module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Grammar
import Parser

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

combine :: TypeEnv -> TypeEnv -> TypeEnv
combine (TypeEnv env1) (TypeEnv env2) = TypeEnv $ env1 `M.union` env2

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (apply s1) s2 `M.union` s1

newtype TypeEnv = TypeEnv (M.Map (Kind, String) Scheme)

instance Show TypeEnv where
    show (TypeEnv env) = show env

emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty

remove :: TypeEnv -> Kind -> String -> TypeEnv
remove (TypeEnv env) k var = TypeEnv $ M.delete (k, var) env

removeAll :: TypeEnv -> Kind -> [String] -> TypeEnv
removeAll env _ [] = env
removeAll env k (v:vars) = removeAll (remove env k v) k vars 

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
runTI t = runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
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

tiSPL :: TypeEnv -> SPL -> TI (Subst, Type, TypeEnv)
tiSPL env (SPL ds) = do
    (s, t, e) <- tiDecls env ds
    return (s, t, apply s e)

tiDecl :: TypeEnv -> Decl -> TI (Subst, Type, TypeEnv)
tiDecl env (DeclVarDecl v) = tiVarDecl env v
tiDecl e (DeclFunDecl f) = tiFunDecl e f

tiVarDecl :: TypeEnv -> VarDecl -> TI (Subst, Type, TypeEnv)
tiVarDecl env (VarDeclVar s e) = do
   tv <- newTyVar "a"
   let TypeEnv env' = remove env Var s
   let env'' = TypeEnv (env' `M.union` M.singleton (Var, s) (Scheme [] tv))
   (s, t) <- tiExp env'' e
   return (s, t, apply s env'')
tiVarDecl (TypeEnv env) (VarDeclType t s e) = do
    let env' = TypeEnv (env `M.union` M.singleton (Var, s) (Scheme [] t))
    (s1, t1) <- tiExp env' e
    s2 <- mgu t t1
    return (s1 `composeSubst` s2, t, apply (s1 `composeSubst` s2) env')

tiDecls :: TypeEnv -> [Decl] -> TI (Subst, Type, TypeEnv)
tiDecls env [] = return (nullSubst, End, emptyEnv)
tiDecls env (d:ds) = do
    (s1, t1, e1) <- tiDecl env d
    (s2, t2, e2) <- tiDecls (apply s1 env) ds
    return (s1 `composeSubst` s2, t2, e1 `combine` e2)

nameOfVarDecl :: VarDecl -> String
nameOfVarDecl (VarDeclVar n _) = n
nameOfVarDecl (VarDeclType _ n _) = n

-- Misschien moeten we ipv TI (Subst, Type) TI (Subst, Type, TypeEnv) returnen overal
-- Als je de λ-calculus wil typeren, heb je geen state, maar hier wel.

tiVarDecls :: TypeEnv -> [VarDecl] -> TI (Subst, Type, TypeEnv)
tiVarDecls _ [] = return (nullSubst, Void, emptyEnv)
tiVarDecls e@(TypeEnv env) (v:vs) = do
    let n = nameOfVarDecl v
    (s1, t1, e1) <- tiVarDecl e v
    let env' = TypeEnv $ env `M.union` M.singleton (Var, n) (Scheme [] t1)
    (s2, t2, e2) <- tiVarDecls (apply s1 env') vs
    return (s1 `composeSubst` s2, t2, env')

tiFunDecl :: TypeEnv -> FunDecl -> TI (Subst, Type, TypeEnv)
tiFunDecl env (FunDecl _ _ (Just t) _ _) = return (nullSubst, t, emptyEnv)
-- tiFunDecl env (FunDecl _ args Nothing vars stmts) = do
--     argsTv <- mapM newTyVar args
--     retTv <- newTyVar "r"
--     let TypeEnv env' = removeAll env Var args
--     let argsTvMap = M.fromList $ zipWith (\a t -> ((Var, a), Scheme [] t)) args argsTv
--     let env'' = TypeEnv (env' `M.union` argsTvMap)
--     -- check return
--     -- check var decl
--     -- check statms

--     return _a

-- FunDecl zonder type

tiStmts :: TypeEnv -> [Stmt] -> TI (Subst, Type)
tiStmts env (s:ss) = do
    (s1, t1) <- tiStmt env s
    (s2, t2) <- tiStmts (apply s1 env) ss
    return (s1 `composeSubst` s2, t1)

checkField :: Type -> Field -> TI Type
checkField (TypeArray t) Head = return t
checkField t@(TypeArray ty) Tail = return t
checkField (TypeTuple t1 t2) First = return t1
checkField (TypeTuple t1 t2) Second = return t2
checkField n f = throwError $ show f ++ " cannot be applied on type " ++ show n

checkFields :: Type -> [Field] -> TI Type
checkFields = foldM checkField

tiStmt :: TypeEnv -> Stmt -> TI (Subst, Type)
tiStmt env (StmtIf e ss1 ss2) = do
    (s1, t1) <- tiExp env e
    s1 <- mgu (TypeBasic BoolType) t1
    (s2, t2) <- tiStmts env ss1
    (s3, t3) <- tiStmts (apply s1 env) (fromMaybe [] ss2)
    return (s1 `composeSubst` s2 `composeSubst` s3, t2)
tiStmt env (StmtWhile e ss) = do
    (s1, t1) <- tiExp env e
    s1 <- mgu (TypeBasic BoolType) t1
    (s2, t2) <- tiStmts env ss
    return (s1 `composeSubst` s2, t2)
tiStmt e@(TypeEnv env) (StmtField n fs ex) = do
    (s1, t1) <- tiExp e ex
    case M.lookup (Var, n) env of
        Nothing -> throwError $ n ++ " is not defined"
        Just sigma -> do
            t <- instantiate sigma
            t' <- checkFields t fs
            s2 <- mgu t1 t'
            return (s1 `composeSubst` s2, t)
tiStmt env (StmtFunCall f) = tiFunCall env f
tiStmt env (StmtReturn sr) = do
    case sr of
        Nothing -> return (nullSubst, Void)
        Just e -> do
            tiExp env e

retType :: Type -> Type
retType (TypeFun t End) = t
retType (TypeFun t1 t2) = retType t2

funType :: Type -> Type
funType (TypeFun t End) = End
funType (TypeFun t1 t2) = TypeFun t1 (funType t2)

tiFunCall :: TypeEnv -> FunCall -> TI (Subst, Type)
tiFunCall e@(TypeEnv env) (FunCall n es) = case M.lookup (Fun, n) env of
    Nothing -> throwError $ "function " ++ n ++ " doesn't exist"
    Just sigma -> do
        t <- instantiate sigma
        ts <- mapM (tiExp e) es
        let t1 = foldr (TypeFun . snd) End ts
        let s1 = foldr (composeSubst . fst) nullSubst ts
        let ret = retType t
        let fun = funType t
        s <- mgu t1 fun
        return (s, ret)

-- We moeten iets aan type toevoegen/ veranderen/ nieuw data type toevoegen
-- dat ervoor zorgt dat we a -> b kunnen doen (functie types)

tiOp1 :: Op1 -> (Type, Type)
tiOp1 Min = (TypeBasic IntType, TypeBasic IntType)
tiOp1 Not = (TypeBasic BoolType, TypeBasic BoolType)

tiOp2 :: Op2 -> (Type, Type, Type)
tiOp2 o
    | o `elem` [Plus, Minus, Product, Division, Modulo] = (TypeBasic IntType, TypeBasic IntType, TypeBasic IntType)
    | o `elem` [And, Or] = (TypeBasic BoolType, TypeBasic BoolType, TypeBasic BoolType)
    | otherwise = undefined

tiExp :: TypeEnv -> Exp -> TI (Subst, Type)
tiExp env (Exp o e1 e2) = do
    let (t1, t2, t3) = tiOp2 o
    (s1, t1') <- tiExp env e1
    (s2, t2') <- tiExp (apply s1 env) e2
    s3 <- mgu t1 t1'
    s4 <- mgu t2 t2'
    return (s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4, t3)
tiExp env (ExpOp1 o e) = do
    let (t1, t2) = tiOp1 o
    (s1, t1') <- tiExp env e
    s2 <- mgu t1 t1'
    return (s1 `composeSubst` s2, t2)
tiExp env (ExpTuple (e1, e2)) = do
    (s1, t1) <- tiExp env e1
    (s2, t2) <- tiExp (apply s1 env) e1
    return (s1 `composeSubst` s2, TypeTuple t1 t2)
tiExp env (ExpBrackets e) = tiExp env e
tiExp (TypeEnv env) (ExpField s fs) = case M.lookup (Var, s) env of
    Nothing -> throwError $ s ++ " is not defined"
    Just sigma -> do 
        t <- instantiate sigma
        t' <- checkFields t fs
        s <- mgu t t'
        return (s, t')
tiExp _ (ExpInt _) = return (nullSubst, TypeBasic IntType)
tiExp _ (ExpBool _) = return (nullSubst, TypeBasic BoolType)
tiExp _ (ExpChar _) = return (nullSubst, TypeBasic CharType)
tiExp env (ExpFunCall f) = tiFunCall env f
tiExp _ ExpEmptyList = do
    t <- newTyVar "l"
    return (nullSubst, TypeArray t)

-- ti :: TypeEnv -> Decl -> TI Type
-- ti env e = do
--     (s,t) <- tiDecl env e
--     return (apply s t)

testExp :: Exp -> IO ()
testExp e = do
    (res, _) <- runTI $ tiExp (TypeEnv M.empty) e
    case res of
        Left err -> putStrLn $ "error: " ++ err
        Right t -> putStrLn $ show e ++ " :: " ++ show t

test' :: [VarDecl] -> IO ()
test' ds = do
    (res, _) <- runTI $ tiVarDecls (TypeEnv M.empty) ds
    case res of
        Left err -> putStrLn $ "error: " ++ err
        Right t -> putStrLn $ show ds ++ " :: " ++ show t

-- showSubst :: TI (Subst, Type) -> IO ()
-- showSubst ti = do
--     (a, b) <- runTI ti
--     case a of
--         Left err -> putStrLn  err
--         Right (s, t) -> print s

-- showSubst' :: TI Subst -> IO ()
-- showSubst' ti = do
--     (a, b) <- runTI ti
--     case a of
--         Left err -> putStrLn  err
--         Right s -> print s

-- test :: Decl -> IO ()
-- test d = do
--     (res, _) <- runTI (ti (TypeEnv M.empty) d)
--     case res of
--         Left err -> putStrLn $ "error: " ++ err
--         Right t -> putStrLn $ show d ++ " :: " ++ show t