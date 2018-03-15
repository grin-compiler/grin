{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor #-}
module Transformations.Simplifying.RightHoistFetch where

import Data.Function
import Data.Map (Map)
import Debug.Trace (traceShowId, trace)
import Grin
import PrimOps
import Transformations.Rename
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Data.Functor.Infix
import Control.Monad.Free
import Text.Show.Pretty (ppShow)
import Text.Printf


rightHoistFetch :: Exp -> Exp
rightHoistFetch = solve rightHoistFetchOneStep

rightHoistFetchOneStep :: Exp -> Exp
rightHoistFetchOneStep e =
  apo builder (Map.empty, [], e)
  where
    vars0' = rightHoistFetchVars e
    vars0 = trace (ppShow vars0') vars0'
    vars = firstUsed vars0
    nonLocal = usedInDifferentBlock vars0

    builder :: (Map Name SimpleExp, [Step], Exp) -> ExpF (Either Exp (Map Name SimpleExp, [Step], Exp))
    builder (moves, path, e) = traceShowId $ case e of

      EBind fetch@(SFetchI n p) pval@(Var name) rest ->
        let names0 = foldr (++) [] (NamesInExpF (project fetch) list)
            names1 = foldNames list pval
            names = names0 ++ names1
            shouldMove = not . Map.null $ Map.filterWithKey (\k _ -> k `elem` names1) $ monoidMap nonLocal
            shouldInsert = intersection' names0 moves
            moves' = if shouldMove then (Map.insert name fetch moves) else moves
            moves'' = Map.difference moves' shouldInsert
            lastBind = if shouldMove then rest else (EBind fetch pval rest)
            renamings = Map.fromList $
              map (\n -> (n, n ++ "." ++ show (findUsedNameIdx n (reverse path) vars)))
                  (Map.keys shouldInsert)
        in
            bindF moves'' path . renameNames renamings $ Map.foldrWithKey
              (\n se b -> EBind se (Var n) b)
              lastBind
              shouldInsert

      EBind se pval rest ->
        let names0 = foldr (++) [] (NamesInExpF (project se) list)
            names1 = foldNames list pval
            names = names0 ++ names1
            shouldInsert = intersection' names0 moves
            moves' = Map.difference moves shouldInsert
            renamings = Map.fromList $
              map (\n -> (n, n ++ "." ++ show (findUsedNameIdx n (reverse path) vars)))
                  (Map.keys shouldInsert)
        in
            bindF moves' path . renameNames renamings $ Map.foldrWithKey
              (\n se b -> EBind se (Var n) b)
              (EBind se pval rest)
              shouldInsert

      ECase val alts ->
        ECaseF val $ zipWith
          (\i e -> (Right (moves, [CAlt i, Case (length alts)] ++ path, e)))
          [1..] alts

      Def def args body -> DefF def args (Right (moves, SName def:path, body))

      e | isSimpleExp e ->
          let names = foldr (++) [] (NamesInExpF (project e) list)
              shouldInsert = intersection' names moves
              moves' = Map.difference moves shouldInsert
              renamings = Map.fromList $
                map (\n -> (n, n ++ "." ++ show (findUsedNameIdx n (reverse path) vars)))
                    (Map.keys shouldInsert)
          in
              bindF moves' path . renameNames renamings $ Map.foldrWithKey
                (\n se b -> EBind se (Var n) b)
                e
                shouldInsert


      e -> (Right . (,,) moves path) <$> project e
      where
        -- Does not recurse on EBind simple expr
        bindF m p (EBind se pval rest) =
          EBindF (Left se) pval (Right (m, p, rest))

        bindF m p (ECase val alts) = -- This is a hack...
          ECaseF val $ zipWith
            (\i e -> (Right (m, [CAlt i, Case (length alts)] ++ p, e)))
            [1..] alts

        bindF m p e = Right . (,,) m p <$> project e

        list x = [x]
        namesInExp = foldr (:) [] (NamesInExpF (project e) list)
        containsKeys keys = not . Map.null . Map.filterWithKey (\k _ -> k `elem` keys)
        intersection' keys m = Map.intersection m (Map.fromList $ zip keys [1..])


-- For local blocks...
rightHoistFetchVars :: Exp -> RHIData
rightHoistFetchVars = cata collect where
  collect :: ExpF RHIData -> RHIData
  collect = \case
    EBindF se pval rest -> mconcat [se, rest, defNameVal pval]
    ECaseF val alts -> mconcat $ useNameVal val :
                               [ ([Case caseSize, CAlt i] ++) <$$$> alt
                               | (i, alt) <- zip [1..] alts
                               ]
                               where caseSize = length alts

    -- QUESTION: should the definition name be registered as defined also?
    DefF name args body -> nubRHIData $ (SName name:) <$$$> mconcat (body : map defName args)

    -- Does not collect function names.
    SAppF _name args  -> mconcat $ map useNameVal args

    SReturnF val      -> useNameVal val
    SStoreF val       -> useNameVal val
    SFetchIF name pos -> useName name
    SUpdateF name val -> mconcat [useName name, useNameVal val]

    e -> Data.Foldable.fold e

  collectNames = foldNames pure
  useNameVal = mconcat . map useName . collectNames
  defNameVal = mconcat . map defName . collectNames
  useName n = MonoidMap $ Map.singleton n [Used []]
  defName n = MonoidMap $ Map.singleton n [Defined []]



data VarOccurance a
  = Defined a
  | Used    a
  deriving (Eq, Functor, Show, Ord)

coPoint :: VarOccurance a -> a
coPoint = \case
  Defined a -> a
  Used    a -> a

isDefined :: VarOccurance a -> Bool
isDefined = \case
  Defined _ -> True
  Used    _ -> False

isUsed :: VarOccurance a -> Bool
isUsed = \case
  Defined _ -> False
  Used    _ -> True

data Step
  = SName Name
  | Case Int
  | CAlt Int
  deriving (Eq, Ord, Show)

type Path = [Step]

newtype MonoidMap k v = MonoidMap {monoidMap :: Map k v} deriving (Functor, Show)
instance (Monoid v, Ord k) => Monoid (MonoidMap k v) where
  mempty = MonoidMap mempty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap $ Map.unionWith mappend a b

type RHIData = MonoidMap Name [VarOccurance Path]

-- | Returns the map of the location where the name is used
-- ordered by the location.
firstUsed :: RHIData -> RHIData
firstUsed (MonoidMap a) = MonoidMap $ Map.map (List.sort . filter isUsed) a

-- | Remove duplicates
nubRHIData :: RHIData -> RHIData
nubRHIData (MonoidMap a) = MonoidMap $ Map.map List.nub a

-- | Find the unique index of name in the usage path.
findUsedNameIdx :: Name -> Path -> RHIData -> Int
findUsedNameIdx n p (MonoidMap m) = case Map.lookup n m of
  Nothing -> error $ printf "Impossible: name must be in map\nname = %s\npath = %s\nrhidata = %s" n (ppShow p) (ppShow m)
  Just us -> case List.findIndex ((p ==) . coPoint) us of
    Nothing -> error $ printf "Impossible: path must be in the list\nname = %s\npath = %s\nus = %s\nrhidata = %s" n (ppShow p) (ppShow us) (ppShow m)
    Just ix -> ix

-- | Returns the variables that are not used where they are defined.
usedInDifferentBlock :: RHIData -> RHIData
usedInDifferentBlock (MonoidMap rhid) = MonoidMap $ Map.filterWithKey nonLocallyUsed rhid where
  nonLocallyUsed n vs = Map.member n primOps || case (List.filter isDefined vs) of
    []    -> error $ "Undefined variable: " ++ n ++ " " ++ show vs
    [Defined path] -> maybe True (const False) $ List.find (path==)
                      $ map coPoint
                      $ List.filter isUsed vs
    bad -> error $ "Mupliple defined variable: " ++ show bad

solve t = fix (\rec v -> let tv = t v in if tv == v then tv else rec tv)

-- Second implementation

data CBlock
  = CBDef Name
  | CBCase Val Int
  | CBBlock
  | CBAlt CPat
  deriving (Eq, Show)

path :: ([CBlock], Exp) -> ExpF ([CBlock], Exp)
path (p, e) = case e of
  Program  defs -> ProgramF ((,) p <$> defs)
  Def      name params body -> DefF name params (CBDef name:p, body)
  -- Exp
  EBind    se lpat rest -> EBindF (p,se) lpat (p, rest)
  ECase    val alts -> ECaseF val ((,) (CBCase val (length alts):p) <$> alts)
  -- Simple Expr
  SApp     name params -> SAppF name params
  SReturn  val -> SReturnF val
  SStore   val -> SStoreF val
  SFetchI  name pos -> SFetchIF name pos
  SUpdate  name val -> SUpdateF name val
  SBlock   rest -> SBlockF (CBBlock:p, rest)
  -- Alt
  Alt cpat body -> AltF cpat ((CBAlt cpat):p, body)

debugPath :: Exp -> Exp
debugPath = ana (dCoAlg (show . fst) path) . ((,) [])

rhf :: Exp -> Exp
rhf e = futu (dCoAlg (show . fst) rhfca) ([], e) where
  rhfca :: ([Name], Exp) -> ExpF (Free ExpF ([Name], Exp))
  rhfca (ns, e) = case e of
    Program  defs -> ProgramF (pureF . (,) ns <$> defs)
    Def      name params body -> DefF name params (pureF (concat [[name], params, ns], body))
    -- Exp
    EBind    se lpat rest -> EBindF (pureF (ns, se)) lpat (pureF (ns, rest))
    ECase    val alts -> ECaseF val (pureF . (,) ns <$> alts)
    -- Simple Expr
    SApp     name params -> SAppF name params
    SReturn  val -> SReturnF val
    SStore   val -> SStoreF val
    SFetchI  name pos -> SFetchIF name pos
    SUpdate  name val -> SUpdateF name val
    SBlock   rest -> SBlockF (pureF (ns, rest))
    -- Alt
    Alt cpat body -> AltF cpat (pureF (ns, body))
  pureF :: a -> Free ExpF a
  pureF = return

{-
copyList :: [a] -> [a]
copyList = futu coAlg where
  coAlg :: [a] -> ListF a (Free (ListF a) [a])
  coAlg = \case
    [] -> Nil
    (x:xs) -> Cons x $ return xs

non3 :: [Int] -> [Int]
non3 = futu coAlg where
  coAlg :: [Int] -> ListF Int (Free (ListF Int) [Int])
  coAlg = \case
    [] -> Nil
    (x:xs) -> Cons x $ return $
      case xs of
        [] -> []
        (y:ys) | y == 3    -> ys
               | otherwise -> y:ys
nil :: Free (ListF a) b
nil = liftF Nil

cons :: a -> b -> Free (ListF a) b
cons h t = liftF (Cons h t)

-- BAD IMPEMENTATION!
twiddle :: [a] -> [a]
twiddle = ana coAlg where
  coAlg :: [a] -> ListF a [a]
  coAlg = \case
    []     -> Nil
    (x:l) -> case l of
      []    -> Cons x []
      (h:t) -> Cons h (x:t)

twiddle2 :: [a] -> [a]
twiddle2 = futu coalg where
  coalg :: [a] -> ListF a (Free (ListF a) [a])
  coalg r = case r of
    []    -> Nil
    (x:l) -> case l of
      []    -> Cons x nil
      (h:t) -> Cons h $ cons x t
-}
