{-# LANGUAGE LambdaCase, EmptyCase, ViewPatterns #-}
module Transformations.Optimising.CaseCopyPropagation (caseCopyPropagation) where

import Control.Arrow
import Grin
import Data.Functor.Foldable
import Data.Monoid hiding (Alt)
import Debug.Trace
import Data.Maybe
import Data.List (find)
import Lens.Micro.Platform hiding (zoom)
import TypeEnv
import qualified Data.Vector
import qualified Data.Map as Map

{-
When implementing the transformation we need to, for each case expression:
1. Examine the return sites of all the alternatives
2. Decide if the transformation applies (all returns must be similar units)
3. Possible transform all the return sites
4. Continue transforming the code in each alternative

The transformation can eve be done in a single pass and linear time,
using techniques for circular algorithms.
[Bir84, Joh87a]

Bir 84: R S Bird. Using Circular Programs to Eliminate Multiple Traversals of Data. 1984
Joh87a: T. Johnson. Attribute Grammars as a Functional Programming Paradigm. 1987
-}

-- * Collection

type Step = ExpF ()
type Path = [Step]

data Info = Info
  { returns :: [(Path, Maybe (Tag, SimpleType))]
  , cases   :: [(Path, (Tag, SimpleType))]
  }
  deriving Show

instance Monoid Info where
  mempty = Info [] []
  mappend (Info r1 c1) (Info r2 c2) = Info (r1 <> r2) (c1 <> c2)

toStep :: ExpF a -> Step
toStep = fmap (const ())

isEmptyInfo :: Info -> Bool
isEmptyInfo (Info returns cases) = null returns && null cases

addStep :: Step -> Info -> Info
addStep s (Info returns cases) = Info
  (map (\(p, v) -> ((s:p), v)) returns)
  (map (\(p, v) -> ((s:p), v)) cases)

stepInside :: Step -> Info -> Info
stepInside s (Info returns cases) = Info
  (map (first tail) $ filter (([s] ==) . take 1 . fst) returns)
  (map (first tail) $ filter (([s] ==) . take 1 . fst) cases)

zoom :: Int -> Info -> Info
zoom n (Info returns cases) = Info (map (first (drop n)) returns) (map (first (drop n)) cases)

caseTagOnStep :: Step -> Info -> Maybe Tag
caseTagOnStep s (Info returns cases) = fmap (fst . snd) $ find (([s] ==) . fst) $ cases

caseFocusOnStep :: Step -> Info -> Bool
caseFocusOnStep s i = isJust $ caseTagOnStep s i

typeOf :: TypeEnv -> Val -> SimpleType
typeOf env = \case
  (Lit l) -> typeOfLitST l
  (Var v) -> case env ^. variable . at v of
    Nothing -> error $ "Variable is not defined: " <> v
    Just (T_SimpleType t) -> t
  bad -> error $ show bad

collectInfo :: TypeEnv -> Exp -> Info
collectInfo env = para convert where
  convert :: ExpF (Exp, Info) -> Info
  convert e = addStep (toStep e) $ case e of
    EBindF (SBlock _, se) _ (_, r) -> se <> r
    EBindF _ _ (EBind _ _ _, r)    -> r
    EBindF _ _ (ECase _ _, r)      -> r

    EBindF _ _ (SReturn (ConstTagNode tag@(Tag _ _) [v]), _) -> mempty { returns = [(mempty, Just (tag, typeOf env v))] } -- Good node
    EBindF _ _ (SReturn _, _)                                -> mempty { returns = [(mempty, Nothing)] }  -- Bad node

    ProgramF (mconcat . map snd -> info) -> info
    DefF _ _ (_, info)                   -> info { returns = mempty }

    ECaseF v ((mconcat . map snd) -> info) -> case info of
      Info returns cases -> case (allTheSame $ map snd returns) of
        Just (Just x) -> Info mempty (([], x):cases) -- The case is the same return values
        Just Nothing  -> Info mempty cases
        Nothing       -> Info mempty cases

    AltF pat (ei, info) -> info
    SBlockF (ei, info)  -> info

    _ -> mempty

-- * Build

extendTypeEnv :: Info -> TypeEnv -> TypeEnv
extendTypeEnv (Info returns cases) te = foldl addVar te cases where
  addVar te0 (path, (tag, typ)) = case (last path) of
    ECaseF v@(Var n) _ -> extend te0 $ newVar (n <> "'") (T_SimpleType typ)
    bad                -> error $ show bad

data BuilderState
  = Build Exp Info Bool
  | Skip  Int Exp Info Bool

caseCopyPropagation :: (TypeEnv, Exp) -> (TypeEnv, Exp)
caseCopyPropagation (env, e) = (extendTypeEnv info env, apo builder (Build e info False)) where

  info = collectInfo env e

  builder :: BuilderState -> ExpF (Either Exp BuilderState)

  -- Skip some expressions without transforming them.
  builder (Skip 0 e0 i r) = builder (Build e0 i r)
  builder (Skip n e0 i r) = fmap (Right . (\e1 -> (Skip (n-1) e1 i r))) $ project e0

  -- Real transformation.
  builder (Build e i False) | isEmptyInfo i = fmap Left $ project e
  builder (Build e i rewrite) = case e of
    -- Just step in in the followings
    Program     defs -> ProgramF (map stepIn defs)
    Def n args body  -> DefF n args (stepIn body)
    SBlock body      -> SBlockF $ stepIn body
    Alt cpat body    -> AltF cpat (stepIn body)

    -- Exp: Insert nodes, skips them in the recursion and continue the transformation on the Case node.
    EBind lhs@(SBlock cs@(ECase var@(Var n) alts)) pat rhs
      | caseFocusOnStep (toStepE cs) i1 ->
          EBindF
            (Right $ Skip 3
              (SBlock (EBind lhs newVar (SReturn (ConstTagNode (fromJust (caseTagOnStep (toStepE cs) i1)) [newVar]))))
              i1
              True)
            pat
            (stepIn rhs)
      | otherwise -> EBindF (stepInF lhs) pat (stepIn rhs)
      where
        i1 = zoom 2 i
        newVar = Var (n <> "'")

    -- Exp: The last statement is a case
    EBind lhs pat rhs@(ECase var@(Var n) alts)
      | caseFocusOnStep (toStepE rhs) i1 ->
          EBindF
            (stepIn lhs)
            pat
            (Right $ Skip 3
              (EBind (SBlock rhs) newVar (SReturn (ConstTagNode (fromJust (caseTagOnStep (toStepE rhs) i1)) [newVar])))
              i1
              True)
      | otherwise -> EBindF (stepIn lhs) pat (stepIn rhs)
      where
        i1 = zoom 1 i
        newVar = Var (n <> "'")

    EBind lhs pat rhs -> EBindF (stepIn lhs) pat (stepIn rhs)

    ECase val alts
      | caseFocusOnStep step i -> ECaseF val (map stepInT alts)
      | otherwise              -> ECaseF val (map stepIn alts)

    r@(SReturn (ConstTagNode (Tag _ _) [v]))
      | rewrite   -> SReturnF v
      | otherwise -> Left <$> project r

    -- Simple Exp
    rest -> Left <$> project rest
    where
      stepIn  e0 = Right (Build e0 (stepInside step i) rewrite)
      stepInT e0 = Right (Build e0 (stepInside step i) True)
      stepInF e0 = Right (Build e0 (stepInside step i) False)
      step = toStepE e

      toStepE :: Exp -> Step
      toStepE = toStep . project

-- * Utils

allTheSame :: (Eq a) => [a] -> Maybe a
allTheSame [] = Nothing
allTheSame (x:xs) = if any (/=x) xs then Nothing else Just x
