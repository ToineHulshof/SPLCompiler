{-# LANGUAGE TupleSections #-}

module Types where

import Control.Monad.Except ( zipWithM, runExceptT, MonadError(throwError), ExceptT )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State
import Control.Monad.Writer hiding ( Product, First )
import Data.Maybe ( fromMaybe, listToMaybe, isJust, isNothing, mapMaybe )
import Data.List ( group, sort, elemIndex )
import qualified Data.Map as M
import qualified Data.Set as S
import Grammar
import Errors
import Printer ()
import Data.List.NonEmpty ( NonEmpty((:|)), cons )
import qualified Data.List.NonEmpty as NE
import Parser ( expToP )
import Debug.Trace ( trace )

data Scheme = Scheme [String] Type deriving (Show)
type Subst = M.Map String Type
data Kind = Var | Fun deriving (Eq, Ord)

instance Show Kind where
    show Var = "\x1b[35mVar\x1b[0m"
    show Fun = "\x1b[1m\x1b[35mFun\x1b[0m"

class Types a where
  ftv :: a -> S.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TypeID _ n) = S.singleton n
  ftv (TypeBasic _)  = S.empty
  ftv (TypeList t) = ftv t
  ftv (TypeFun t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TypeTuple t1 t2)  = ftv t1 `S.union` ftv t2
  ftv Void = S.empty

  apply s (TypeID c n) = case M.lookup n s of
    Nothing -> TypeID c n
    Just t -> checkConditions t
        where
            checkConditions (TypeID c1 n) = TypeID (composeConditions c c1) n
            checkConditions t = t
  apply _ (TypeBasic t) = TypeBasic t
  apply s (TypeList t) = TypeList (apply s t)
  apply s (TypeFun t1 t2) = TypeFun (apply s t1) (apply s t2)
  apply s (TypeTuple t1 t2) = TypeTuple (apply s t1) (apply s t2)
  apply _ Void = Void

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv l = foldr (S.union . ftv) S.empty l

instance Types a => Types (Maybe a) where
    apply s a = apply s <$> a
    ftv = undefined

nullSubst :: Subst
nullSubst = M.empty

combine :: TypeEnv -> TypeEnv -> TypeEnv
combine (TypeEnv env1) (TypeEnv env2) = TypeEnv $ env1 `M.union` env2

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (apply s1) s2 `M.union` s1

newtype TypeEnv = TypeEnv (M.Map (Kind, String) Scheme)

instance Eq TypeEnv where
    e1 == e2 = show e1 == show e2

conditions :: Type -> [(String, Condition)]
conditions (TypeBasic _) = []
conditions (TypeTuple t1 t2) = conditions t1 ++ conditions t2
conditions (TypeList t) = conditions t
conditions (TypeID Nothing _) = []
conditions (TypeID (Just c) n) = [(n, c)]
conditions (TypeFun t1 t2) = conditions t1 ++ conditions t2
conditions Void = []

showConditions :: M.Map String String -> [(String, Condition)] -> String
showConditions m [] = ""
showConditions m [(s, c)] = "\x1b[34m" ++ show c ++ "\x1b[0m \x1b[36m" ++ fromMaybe s (M.lookup s m) ++ "\x1b[0m => "
showConditions m ((s, c):cs) = "\x1b[34m" ++ show c ++ "\x1b[0m \x1b[36m" ++ fromMaybe s (M.lookup s m) ++ "\x1b[0m, " ++ showConditions m cs 

varStrings :: Type -> [String]
varStrings (TypeID _ s) = [s]
varStrings (TypeTuple t1 t2) = varStrings t1 ++ varStrings t2
varStrings (TypeList t) = varStrings t
varStrings (TypeFun t1 t2) = varStrings t1 ++ varStrings t2
varStrings _ = []

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

varsMap :: Type -> M.Map String String
varsMap t = M.fromList $ zip (reverse $ removeDuplicates $ reverse $ varStrings t) (map (: []) ['a' .. 'z'])

showType :: Bool -> M.Map String String -> Type -> String
showType f m (TypeBasic b) = show b
showType f m (TypeTuple t1 t2) = "(" ++ showType f m t1 ++ (if f then "\x1b[1m" else "") ++ ", " ++ showType f m t2 ++ (if f then "\x1b[1m" else "") ++ ")\x1b[0m"
showType f m (TypeList t) = (if f then "\x1b[1m" else "") ++ "[" ++ showType f m t ++ (if f then "\x1b[1m" else "") ++ "]\x1b[0m"
showType f m (TypeID _ s) = "\x1b[36m" ++ fromMaybe s (M.lookup s m) ++ "\x1b[0m"
showType f m (TypeFun t1 t2) = showType f m t1 ++ (if f then "\x1b[1m" else "") ++ " -> " ++ showType f m t2
showType f m Void = "\x1b[34mVoid\x1b[0m"

instance Show TypeEnv where
    show env = help funs ++ help vars
        where
            help (TypeEnv e) = unlines $ map (\((k, n), Scheme _ t) -> show k ++ " " ++ "\x1b[33m" ++ n ++ "\x1b[0m" ++ " :: " ++ showConditions (varsMap t) (map head $ group $ sort $ conditions t) ++ showType False (varsMap t) t) $ filter (\((_, n), _) -> n `notElem` ["print", "isEmpty"]) $ M.toList e
            (funs, vars) = woFun env

emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty

remove :: TypeEnv -> Kind -> String -> TypeEnv
remove (TypeEnv env) k var = TypeEnv $ M.delete (k, var) env

removeAll :: TypeEnv -> Kind -> [String] -> TypeEnv
removeAll env _ [] = env
removeAll env k (v:vars) = removeAll (remove env k v) k vars

woFun :: TypeEnv -> (TypeEnv, TypeEnv)
woFun (TypeEnv env) = (help Fun, help Var)
    where
        help :: Kind -> TypeEnv
        help k = TypeEnv $ M.fromList $ filter (\((k1, _), _) -> k1 == k) (M.toList env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (M.elems env)
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `S.difference` S.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)

data TIState = TIState { tiSupply :: Int, tiSubst :: Subst, rType :: Maybe Type, info :: [String] }

type TI a = WriterT [Error] (StateT TIState IO) a

runTI :: TI a -> IO ((a, [Error]), TIState)
runTI t = runStateT (runWriterT t) initTIState
    where 
        initTIState = TIState { tiSupply = 1, tiSubst = M.empty, rType = Nothing, info = [] }

updateReturnType :: Type -> P -> Maybe Exp -> TI Subst
updateReturnType t p e = do
    state <- get
    case rType state of
        Nothing -> put state { rType = Just t } >> return nullSubst
        Just t' -> mgu e p t t'

newTyVar :: Maybe Condition -> String -> TI Type
newTyVar c prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    return (TypeID c ("a" ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (newTyVar Nothing) vars
    let s = M.fromList (zip vars nvars)
    return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = S.toList (ftv t `S.difference` ftv env)

addInfo :: String -> TI ()
addInfo s = do
    t <- get
    put t { info = s : info t }

resetInfo :: TI ()
resetInfo = do
    t <- get
    put t { info = [] }

mgu :: Maybe Exp -> P -> Type -> Type -> TI Subst
mgu e p t1 t2 = mgu' e p t1 t2 (nes (p, t1)) (nes t2)

type RecError = NonEmpty (P, Type)

mgu' :: Maybe Exp -> P -> Type -> Type -> RecError -> NonEmpty Type -> TI Subst
mgu' e p (TypeFun l1 r1) (TypeFun l2 r2) ot1 ot2 = do
    s1 <- mgu' e p l1 l2 ((p, l1) `cons` ot1) (l2 `cons` ot2)
    s2 <- mgu' e p (apply s1 r1) (apply s1 r2) ((p, r1) `cons` ot1) (r2 `cons` ot2)
    return $ s2 `composeSubst` s1
mgu' e@(Just (Exp _ Cons e1 e2 _)) p (TypeList t1) (TypeList t2) ot1 ot2 = let p' = expToP e1 in mgu' e p' t1 t2 ((p', t1) `cons` ot1) (t2 `cons` ot2)
mgu' e p (TypeList t1) (TypeList t2) ot1 ot2 = mgu' e p t1 t2 ((p, t1) `cons` ot1) (t2 `cons` ot2)
mgu' (Just (ExpTuple (e1, e2) _)) p (TypeTuple l1 r1) (TypeTuple l2 r2) ot1 ot2 = do
    s1 <- let p' = expToP e1 in mgu' (Just e1) p' l1 l2 ((p', l1) `cons` ot1) (l2 `cons` ot2)
    s2 <- let p' = expToP e2 in mgu' (Just e2) p' (apply s1 r1) (apply s1 r2) ((p', r1) `cons` ot1) (r2 `cons` ot2)
    return $ s2 `composeSubst` s1
mgu' e p (TypeTuple l1 r1) (TypeTuple l2 r2) ot1 ot2 = do
    s1 <- mgu' e p l1 l2 ((p, l1) `cons` ot1) (l2 `cons` ot2)
    s2 <- mgu' e p (apply s1 r1) (apply s1 r2) ((p, r1) `cons` ot1) (r2 `cons` ot2)
    return $ s2 `composeSubst` s1
mgu' e _ (TypeID c1 u1) (TypeID c2 u2) ot1 ot2 = return $ M.singleton u1 (TypeID (composeConditions c1 c2) u2)
mgu' e p (TypeID c u) t ot1 ot2 = varBind p e u c t ot1 ot2
mgu' e p t (TypeID c u) ot1 ot2 = varBind p e u c t ot1 ot2
mgu' e p l@(TypeBasic t1) r@(TypeBasic t2) ot1 ot2
    | t1 == t2 = return nullSubst
    | otherwise = typeError ot1 ot2
mgu' e _ Void Void ot1 ot2 = return nullSubst
mgu' e p t1 t2 ot1 ot2 = typeError ot1 ot2

typeError :: RecError -> NonEmpty Type -> TI Subst
typeError ((p@(_, a), h1) :| t1) (h2 :| t2) = do
    info <- gets info
    tell [Error TypeError (("\x1b[1m\x1b[33m" ++ a ++ "\x1b[0m\x1b[1m has type " ++ showType True (varsMap h1) h1 ++ "\x1b[1m, but is expected to have type " ++ showType True (varsMap h2) h2 ++ "\x1b[1m") :| zipWith extraError t1 t2 ++ info ) (Just p)] >> return nullSubst
    where 
        extraError :: (P, Type) -> Type -> String
        extraError ((_, a), t1) t2 = "\x1b[1m-> Couldn't match expected type " ++ showType True (varsMap t2) t2 ++ "\x1b[0m\x1b[1m with actual type " ++ showType True (varsMap t1) t1 ++ "\x1b[1m in the expression \x1b[0m\x1b[1m\x1b[33m" ++ a ++ "\x1b[0m\x1b[1m"

condition :: Type -> String -> (Maybe Condition, Bool)
condition (TypeID c n) s = (c, n == s)
condition _ _ = (Nothing, False)

composeConditions :: Maybe Condition -> Maybe Condition -> Maybe Condition
composeConditions Nothing c = c
composeConditions (Just Ord) _ = Just Ord
composeConditions c Nothing = c
composeConditions _ c = c
     
varBind :: P -> Maybe Exp -> String -> Maybe Condition -> Type -> RecError -> NonEmpty Type -> TI Subst
varBind _ _ u (Just Eq) t ot1 ot2 = return $ M.singleton u t
varBind p _ u (Just Ord) t ot1 ot2
    | isOrd t = return $ M.singleton u t
    | otherwise = tell [Error TypeError (nes $ "\x1b[1mno instance \x1b[33mOrd\x1b[0m\x1b[1m for " ++ showType True (varsMap t) t) (Just p)] >> return nullSubst
    where
        isOrd (TypeBasic IntType) = True
        isOrd (TypeBasic CharType) = True
        isOrd _ = False
varBind p@(_, a) e u c t ot1 ot2
    | u `S.member` ftv t = typeError ot1 ot2
    | otherwise = return $ M.singleton u t

-- Helper function for replacing the types in a polymorphic function.
-- This function is used in Codegen.hs
subst :: Type -> Type -> Subst
subst (TypeFun l1 r1) (TypeFun l2 r2) = subst l1 l2 `composeSubst` subst r1 r2
subst (TypeList l) (TypeList r) = subst l r
subst (TypeTuple l1 r1) (TypeTuple l2 r2) = subst l1 l2 `composeSubst` subst r1 r2
subst (TypeID _ s) t = M.singleton s t
subst _ _ = nullSubst

tiSPL :: TypeEnv -> SPL -> TI (Subst, TypeEnv, SPL)
tiSPL = tiDecls

tiDecl :: TypeEnv -> Decl -> TI (Subst, TypeEnv, Decl)
tiDecl env (DeclVarDecl v) = fmap DeclVarDecl <$> tiVarDecl env v
tiDecl e (DeclFunDecl f) = do
    (s, _, e, d) <- tiFunDecl e f
    return (s, e, DeclFunDecl d)

tiVarDecl :: TypeEnv -> VarDecl -> TI (Subst, TypeEnv, VarDecl)
tiVarDecl env (VarDecl Nothing s ex) = do
    (s1, t1, ex') <- tiExp env ex
    let env1@(TypeEnv e) = apply s1 env
    let d = VarDecl (Just t1) s ex'
    let TypeEnv env2 = remove env1 Var s
    return (s1, TypeEnv (M.insert (Var, s) (Scheme [] t1) env2), d)
tiVarDecl env (VarDecl (Just t) s e) = do
    (s1, t1, e') <- tiExp env e
    s2 <- mgu (Just e) (expToP e) t1 t
    let cs1 = s2 `composeSubst` s1
    let TypeEnv env1 = remove env Var s
    let env2 = TypeEnv (M.insert (Var, s) (Scheme [] t) env1)
    return (cs1, apply cs1 env2, VarDecl (Just t) s e')

tiDecls :: TypeEnv -> [Decl] -> TI (Subst, TypeEnv, [Decl])
tiDecls env [] = return (nullSubst, env, [])
tiDecls env (d:ds) = do
    (s1, env1, d1) <- tiDecl env d
    resetInfo
    (s2, env2, d2) <- tiDecls env1 ds
    return (s2 `composeSubst` s1, env2, d1:d2)

tiVarDecls :: TypeEnv -> [VarDecl] -> TI (Subst, TypeEnv, [VarDecl])
tiVarDecls env [] = return (nullSubst, env, [])
tiVarDecls env (v:vs) = do
    (s1, env1, d1) <- tiVarDecl env v
    (s2, env2, d2) <- tiVarDecls env1 vs
    return (s2 `composeSubst` s1, env2, d1:d2)

hasReturn :: Stmt -> Bool
hasReturn (StmtIf _ ss1 ss2) = correctReturn ss1 && correctReturn (fromMaybe [] ss2)
hasReturn (StmtWhile _ ss) = correctReturn ss
hasReturn StmtReturn {} = True
hasReturn _ = False

correctReturn :: [Stmt] -> Bool
correctReturn = any hasReturn

updateTypeStmts :: Subst -> [Stmt] -> [Stmt]
updateTypeStmts s = map $ updateTypeStmt s

updateTypeStmt :: Subst -> Stmt -> Stmt
updateTypeStmt s (StmtIf e ss1 ss2) = StmtIf (updateTypeExp s e) (updateTypeStmts s ss1) (updateTypeStmts s <$> ss2)
updateTypeStmt s (StmtWhile e ss) = StmtWhile (updateTypeExp s e) (updateTypeStmts s ss)
updateTypeStmt s (StmtField n fs e p) = StmtField n fs (updateTypeExp s e) p
updateTypeStmt s (StmtFunCall f) = StmtFunCall (updateTypeFunCall s f)
updateTypeStmt s (StmtReturn e p) = StmtReturn (updateTypeExp s <$> e) p

updateTypeVarDecl :: Subst -> VarDecl -> VarDecl 
updateTypeVarDecl s (VarDecl t n e) = VarDecl (apply s t) n (updateTypeExp s e)

updateTypeExp :: Subst -> Exp -> Exp
updateTypeExp s (Exp t o e1 e2 p) = Exp (apply s t) o (updateTypeExp s e1) (updateTypeExp s e2) p
updateTypeExp s (ExpOp1 o e p) = ExpOp1 o (updateTypeExp s e) p
updateTypeExp s (ExpTuple (e1, e2) p) = ExpTuple (updateTypeExp s e1, updateTypeExp s e2) p
updateTypeExp s (ExpBrackets e p) = ExpBrackets (updateTypeExp s e) p
updateTypeExp s (ExpFunCall f p) = ExpFunCall (updateTypeFunCall s f) p
updateTypeExp _ e = e

updateTypeFunCall :: Subst -> FunCall -> FunCall
updateTypeFunCall s (FunCall t n es p) = FunCall (apply s t) n (map (updateTypeExp s) es) p

tiFunDecl :: TypeEnv -> FunDecl -> TI (Subst, Type, TypeEnv, FunDecl)
tiFunDecl env f@(FunDecl o n args (Just t) vars stmts p)
    | l1 /= l2 = tell [Error TypeError (nes $ "\x1b[33m" ++ n ++ "\x1b[0m\x1b[1m got " ++ show l1  ++ " arguments, but expected " ++ show l2 ++ " arguments") (Just p)] >> return (nullSubst, t, env, f)
    | otherwise = do
        (s1, t1, env1, FunDecl o _ _ _ vars' stmts' _) <- tiFunDecl env (FunDecl o n args Nothing vars stmts p)
        s2 <- mgu Nothing p t1 t
        let t2 = apply s2 t
        let env2 = remove env1 Fun n
        let env3 = env2 `combine` TypeEnv (M.singleton (Fun, n) (generalize env2 t2))
        return (s2 `composeSubst` s1, t2, env3, FunDecl o n args (Just t2) vars' stmts' p)
    where
        l1 = length (funTypeToList t) - 1
        l2 = length args
tiFunDecl env@(TypeEnv envt) f@(FunDecl _ n args Nothing vars stmts p) = case M.lookup (Fun, n) envt of
    Nothing -> tell [Error TypeError (nes $ "function " ++ n ++ " was not found in the environment, while it should be present.") (Just p)] >> return (nullSubst, Void, env, f)
    Just s -> do
        funT <- instantiate s
        tvs <- mapM (newTyVar Nothing) args
        state <- get
        put state { rType = Nothing }
        s0 <- mguList nullSubst (map (\x -> (\a b -> (Nothing, a, b)) x p) (init $ funTypeToList funT)) tvs
        let env1 = remove (apply s0 env) Fun n
        let TypeEnv env2 = removeAll env Var args
        let argsTvMap = M.fromList $ zipWith (\a t -> ((Var, a), Scheme [] t)) args tvs
        let env3 = apply s0 (TypeEnv $ env2 `M.union` argsTvMap)
        (s1, env4, vars') <- tiVarDecls env3 vars
        (s2, stmts') <- tiStmts env4 stmts
        let cs1 = s2 `composeSubst` s1
        returnType <- gets rType
        if isJust returnType && not (correctReturn stmts) then tell [Error TypeError (nes "Not every path has a return statment") (Just p)] >> return (cs1, Void, env4, f) else do
        let t = foldr1 TypeFun $ apply cs1 (tvs ++ [fromMaybe Void returnType])
        let env5 = env1 `combine` TypeEnv (M.singleton (Fun, n) (generalize env1 t))
        let vars'' = map (updateTypeVarDecl cs1) vars'
        let stmts'' = updateTypeStmts cs1 stmts'
        let oa = concatMap oaVarDecl vars'' ++ oaStmts stmts''
        let argTypes = funTypeToList t
        return (cs1, t, apply cs1 env5, FunDecl (mapMaybe (`elemIndex` argTypes) oa) n args (Just t) vars'' stmts'' p)

oaStmts :: [Stmt] -> [Type]
oaStmts = concatMap oaStmt

oaStmt :: Stmt -> [Type]
oaStmt (StmtIf e ss1 ss2) = oaExp e ++ oaStmts ss1 ++ oaStmts (fromMaybe [] ss2)
oaStmt (StmtWhile e ss) = oaExp e ++ oaStmts ss
oaStmt (StmtField _ _ e _) = oaExp e
oaStmt (StmtFunCall f) = oaFunCall f
oaStmt (StmtReturn (Just e) _) = oaExp e
oaStmt _ = []

oaVarDecl :: VarDecl -> [Type]
oaVarDecl (VarDecl _ _ e) = oaExp e

oaExp :: Exp -> [Type]
oaExp (Exp (Just t@TypeID {}) Equals e1 e2 p) = [t] ++ oaExp e1 ++ oaExp e2
oaExp (Exp (Just t@TypeID {}) Neq e1 e2 p) = [t] ++ oaExp e1 ++ oaExp e2
oaExp (Exp _ _ e1 e2 _) = oaExp e1 ++ oaExp e2
oaExp (ExpOp1 _ e _) = oaExp e
oaExp (ExpTuple (e1, e2) _) = oaExp e1 ++ oaExp e2
oaExp (ExpBrackets e _) = oaExp e
oaExp (ExpFunCall f _) = oaFunCall f
oaExp _ = []

oaFunCall :: FunCall -> [Type]
oaFunCall (FunCall (Just t) _ es _) = [t' | t'@(TypeID _ s) <- funTypeToList t] ++ concatMap oaExp es

tiStmts :: TypeEnv -> [Stmt] -> TI (Subst, [Stmt])
tiStmts _ [] = return (nullSubst, [])
tiStmts env (s:ss) = do
    (s1, st1) <- tiStmt env s
    resetInfo
    (s2, st2) <- tiStmts (apply s1 env) ss
    return (s2 `composeSubst` s1, st1:st2)

tiField :: Type -> Field -> TI (Subst, Type)
tiField t (Head p) = do
    t1 <- newTyVar Nothing "f"
    s <- mgu Nothing p (TypeList t1) t
    return (s, apply s t1)
tiField t (Tail p) = do
    t1 <- newTyVar Nothing "f"
    s <- mgu Nothing p (TypeList t1) t
    return (s, apply s $ TypeList t1)
tiField t (First p) = do
    t1 <- newTyVar Nothing "f"
    t2 <- newTyVar Nothing "f"
    s <- mgu Nothing p (TypeTuple t1 t2) t
    return (s, apply s t1)
tiField t (Second p) = do
    t1 <- newTyVar Nothing "f"
    t2 <- newTyVar Nothing "f"
    s <- mgu Nothing p (TypeTuple t1 t2) t  
    return (s, apply s t2)

tiFields :: Type -> [Field] -> TI (Subst, Type)
tiFields t [] = return (nullSubst, t)
tiFields t (f:fs) = do
    (s1, t1) <- tiField t f
    (s2, t2) <- tiFields t1 fs
    return (s2 `composeSubst` s1, t2)

tiStmt :: TypeEnv -> Stmt -> TI (Subst, Stmt)
tiStmt env (StmtIf e ss1 ss2) = do
    (s1, t1, e') <- tiExp env e
    s2 <- mgu (Just e) (expToP e) (TypeBasic BoolType) t1
    let cs1 = s2 `composeSubst` s1
    (s3, ss1') <- tiStmts (apply cs1 env) ss1
    let cs2 = s3 `composeSubst` cs1
    (s4, ss2') <- tiStmts (apply cs2 env) (fromMaybe [] ss2)
    return (s4 `composeSubst` cs2, StmtIf e' ss1' (if isNothing ss2 then Nothing else Just ss2'))
tiStmt env (StmtWhile e ss) = do
    (s1, t1, e') <- tiExp env e
    s2 <- mgu (Just e) (expToP e) (TypeBasic BoolType) t1
    let cs1 = s2 `composeSubst` s1
    (s3, stmts') <- tiStmts (apply cs1 env) ss
    return (s3 `composeSubst` cs1, StmtWhile e' stmts')
tiStmt e s@(StmtField n fs ex p) = do
    (s1, t1, e') <- tiExp e ex
    let TypeEnv env1 = apply s1 e
    case M.lookup (Var, n) env1 of
        Nothing -> tell [Error TypeError (nes $ n ++ " is not defined") (Just p)] >> return (nullSubst, s)
        Just sigma -> do
            t <- instantiate sigma
            (s2, t') <- tiFields t fs
            s3 <- mgu (Just ex) (expToP ex) t1 t'
            return (s3 `composeSubst` s2 `composeSubst` s1, StmtField n fs e' p)
tiStmt env (StmtFunCall f) = do
    (s, _, f) <- tiFunCall env f
    return (s, StmtFunCall f)
tiStmt env (StmtReturn sr p) = case sr of
    Nothing -> updateReturnType Void p Nothing >> return (nullSubst, StmtReturn Nothing p)
    Just e -> do
        (s1, t, e') <- tiExp env e
        s2 <- updateReturnType t p (Just e)
        return (s1 `composeSubst` s2, StmtReturn (Just e') p)
 
retType :: Type -> Type
retType (TypeFun t1 t2) = retType t2
retType t = t

funType :: Type -> Maybe Type
funType (TypeFun t1 r@(TypeFun t2 t3)) = TypeFun t1 <$> funType r
funType (TypeFun t1 t2) = Just t1
funType t = Nothing

funTypeToList :: Type -> [Type]
funTypeToList (TypeFun t1 t2) = t1 : funTypeToList t2
funTypeToList t = [t]

tiExps :: TypeEnv -> [Exp] -> TI (Subst, [Type], [Exp])
tiExps _ [] = return (nullSubst, [], [])
tiExps env (e:es) = do
    (s1, t1, e1) <- tiExp env e
    (s2, t2, e2) <- tiExps (apply s1 env) es
    let cs1 = s2 `composeSubst` s1
    return (cs1, t1 : t2, e1 : e2)

mguList :: Subst -> [(Maybe Exp, Type, P)] -> [Type] -> TI Subst
mguList s [] _ = return s
mguList s ((e, a, p):as) (b:bs) = do
    s1 <- mgu e p (apply s a) (apply s b)
    s2 <- mguList s1 as bs
    return $ s1 `composeSubst` s2

tiFunCall :: TypeEnv -> FunCall -> TI (Subst, Type, FunCall)
tiFunCall e@(TypeEnv env) f@(FunCall _ n es p) = case M.lookup (Fun, n) env of
    Nothing -> tell [Error TypeError (nes $ "function " ++ n ++ " doesn't exist") (Just p)] >> return (nullSubst, Void, f)
    Just sigma -> do
        t <- instantiate sigma
        case funType t of 
            Nothing -> if null es then return (nullSubst, retType t, FunCall (Just t) n es p) else tell [Error TypeError (nes $ "\x1b[33m" ++ n ++ "\x1b[0m\x1b[1m got " ++ show (length es)  ++ " arguments, but expected 0 arguments") (Just p)] >> return (nullSubst, t, f)
            Just funT -> if length es /= length (funTypeToList funT) then tell [Error TypeError (nes $ "\x1b[33m" ++ n ++ "\x1b[0m\x1b[1m got " ++ show (length es)  ++ " arguments, but expected " ++ show (length (funTypeToList funT)) ++ " arguments") (Just p)] >> return (nullSubst, t, f) else do
                (s1, ts, es') <- tiExps e es
                s2 <- mguList nullSubst (zip3 (map Just es) ts (map expToP es)) (apply s1 $ funTypeToList funT)
                let cs1 = s2 `composeSubst` s1
                let t' = foldr1 TypeFun $ apply cs1 (ts ++ [retType t])
                return (cs1, apply cs1 $ retType t, FunCall (Just t') n es' p)

tiOp1 :: Op1 -> (Type, Type)
tiOp1 Min = (TypeBasic IntType, TypeBasic IntType)
tiOp1 Not = (TypeBasic BoolType, TypeBasic BoolType)

tiOp2 :: Op2 -> TI (Type, Type, Type)
tiOp2 o
    | o `elem` [Plus, Minus, Product, Division, Modulo] = return (TypeBasic IntType, TypeBasic IntType, TypeBasic IntType)
    | o `elem` [And, Or] = return (TypeBasic BoolType, TypeBasic BoolType, TypeBasic BoolType)
    | o == Cons = newTyVar Nothing "c" >>= (\t -> return (t, TypeList t, TypeList t))
    | o `elem` [Equals, Neq] = newTyVar (Just Eq) "e" >>= (\t -> return (t, t, TypeBasic BoolType))
    | o `elem` [Leq, Geq, Smaller, Greater] = newTyVar (Just Ord) "e" >>= (\t -> return (t, t, TypeBasic BoolType))

tiExp :: TypeEnv -> Exp -> TI (Subst, Type, Exp)
tiExp env (Exp _ o e1 e2 p) = do
    (t1, t2, t3) <- tiOp2 o
    (s1, t1', e1') <- tiExp env e1
    s2 <- mgu (Just e1) (expToP e1) t1' (apply s1 t1)
    let cs1 = s2 `composeSubst` s1
    (s3, t2', e2') <- tiExp (apply cs1 env) e2
    let cs2 = s3 `composeSubst` cs1
    s4 <- mgu (Just e2) (expToP e2) (apply cs2 t2') (apply cs2 t2)
    let cs3 = s4 `composeSubst` cs2
    return (cs3, apply cs3 t3, Exp (Just (apply cs3 t2)) o e1' e2' p)
tiExp env (ExpOp1 o e p) = do
    let (t1, t2) = tiOp1 o
    (s1, t1', e') <- tiExp env e
    s2 <- mgu (Just e) p t1 t1'
    return (s2 `composeSubst` s1, t2, ExpOp1 o e' p)
tiExp env (ExpTuple (e1, e2) p) = do
    (s1, t1, e1') <- tiExp env e1
    (s2, t2, e2') <- tiExp (apply s1 env) e2
    return (s2 `composeSubst` s1, TypeTuple t1 t2, ExpTuple (e1', e2') p)
tiExp env (ExpBrackets e p) = do
    (s, t, e') <- tiExp env e
    return (s, t, ExpBrackets e' p)
tiExp (TypeEnv env) e@(ExpField n fs p) = case M.lookup (Var, n) env of
    Nothing -> tell [Error TypeError (nes $ n ++ " is not defined") (Just p)] >> return (nullSubst, Void, e)
    Just sigma -> do
        t <- instantiate sigma
        (\(s, ty) -> (s, ty, ExpField n fs p)) <$> tiFields t fs
tiExp _ e@(ExpInt _ _) = return (nullSubst, TypeBasic IntType, e)
tiExp _ e@(ExpBool _ _) = return (nullSubst, TypeBasic BoolType, e)
tiExp _ e@(ExpChar _ _) = return (nullSubst, TypeBasic CharType, e)
tiExp env (ExpFunCall f p) = (\(s, t, f') -> (s, t, ExpFunCall f' p)) <$> tiFunCall env f
tiExp _ e@(ExpEmptyList _) = do
    t <- newTyVar Nothing "l"
    return (nullSubst, TypeList t, e)
