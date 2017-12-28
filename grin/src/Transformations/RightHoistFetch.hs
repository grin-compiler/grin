{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor #-}
module Transformations.RightHoistFetch where

import Data.Function
import Data.Map (Map)
import Debug.Trace (traceShowId)
import Grin
import Transformations.Rename
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Data.Functor.Infix
import Control.Monad.Free


rightHoistFetch :: Exp -> Exp
rightHoistFetch = solve rightHoistFetchOneStep

rightHoistFetchOneStep :: Exp -> Exp
rightHoistFetchOneStep e =
  apo builder (Map.empty, [], e)
  where
    vars0 = rightHoistFetchVars e
    vars = firstUsed vars0
    nonLocal = usedInDifferentBlock vars0

    builder :: (Map Name SimpleExp, [Step], Exp) -> ExpF (Either Exp (Map Name SimpleExp, [Step], Exp))
    builder (moves, path, e) = case e of

      EBind fetch@(SFetchI n p) pval@(Var name) rest ->
        let names0 = foldr (++) [] (NamesInExpF (project fetch) list)
            names1 = foldNames list pval
            names = names0 ++ names1
            shouldMove = not . Map.null $ Map.filterWithKey (\k _ -> k `elem` names1) nonLocal
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
    EBindF se pval rest ->
      Map.unionsWith (++)
        [ Map.fromList . map defName $ collectNames pval
        , se
        , rest
        ]
    ECaseF val alts ->
      Map.unionsWith (++)
        [ (Map.unionsWith (++)
            $ zipWith (\i a -> (([Case (length alts), CAlt i]++) <$$$> a)) [1..] alts)
        , useNameValMap val
        ]
    DefF def args rest ->
      nubRHIData $ Map.unionsWith (++)
        [ uncurry Map.singleton $ defName def
        , (SName def:) <$$$$> Map.fromList $ defName <$> args
        , (SName def:) <$$$> rest
        ]

    -- Does not collect function names.
    SAppF name args ->
      Map.fromList $ map useName $ (concat $ collectNames <$> args)

    SReturnF val -> useNameValMap val
    SStoreF val  -> useNameValMap val
    SFetchIF name pos -> uncurry Map.singleton $ useName name
    SUpdateF name val ->
      Map.unionsWith (++)
        [ uncurry Map.singleton $ useName name
        , Map.fromList $ map useName $ collectNames val
        ]

    e -> Data.Foldable.fold e

  useNameValMap = Map.fromList . map useName . collectNames
  collectNames = foldNames list
  list x = [x]
  useName n = (n, [Used []])
  defName n = (n, [Defined []])



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
type RHIData = Map Name [VarOccurance Path]

-- | Returns the map of the location where the name is used
-- ordered by the location.
firstUsed :: RHIData -> RHIData
firstUsed = Map.map (List.sort . filter isUsed)

-- | Remove duplicates
nubRHIData :: RHIData -> RHIData
nubRHIData = Map.map List.nub

-- | Find the unique index of name in the usage path.
findUsedNameIdx :: Name -> Path -> RHIData -> Int
findUsedNameIdx n p m = case Map.lookup n m of
  Nothing -> error $ "Impossible: name must be in map:" ++ show (n,p,m)
  Just us -> case List.findIndex ((p ==) . coPoint) us of
    Nothing -> error $ "Impossible: path must be in the list:" ++ show (n, p, us, m)
    Just ix -> ix

-- | Returns the variables that are not used where they are defined.
usedInDifferentBlock :: RHIData -> RHIData
usedInDifferentBlock = Map.filter nonLocalyUsed where
  nonLocalyUsed vs = case (List.filter isDefined vs) of
    []    -> False -- True -- error "Undefined variable."
    [Defined path] -> maybe True (const False) $ List.find (path==)
                      $ map coPoint
                      $ List.filter isUsed vs
    bad -> error $ "Mupliple defined variable:" ++ show bad

solve t = fix (\rec v -> let tv = t v in if tv == v then tv else rec tv)

-- Second implementation

data CBlock
  = CBDef Name
  | CBCase Int
  | CBBlock
  | CBAlt CPat
  deriving (Eq, Show)

path :: ([CBlock], Exp) -> ExpF ([CBlock], Exp)
path (p, e) = case e of
  Program  defs -> ProgramF ((,) p <$> defs)
  Def      name params body -> DefF name params (CBDef name:p, body)
  -- Exp
  EBind    se lpat rest -> EBindF (p,se) lpat (p, rest)
  ECase    val alts -> ECaseF val ((,) (CBCase (length alts):p) <$> alts)
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
rhf = futu rhfca where
  rhfca :: Exp -> ExpF (Free ExpF Exp)
  rhfca = \case
    Program  defs -> ProgramF undefined
    Def      name params body -> DefF name params undefined
    -- Exp
    EBind    se lpat rest -> EBindF undefined lpat undefined
    ECase    val alts -> ECaseF val undefined
    -- Simple Expr
    SApp     name params -> SAppF name params
    SReturn  val -> SReturnF val
    SStore   val -> SStoreF val
    SFetchI  name pos -> SFetchIF name pos
    SUpdate  name val -> SUpdateF name val
    SBlock   rest -> SBlockF undefined
    -- Alt
    Alt cpat body -> AltF cpat undefined

