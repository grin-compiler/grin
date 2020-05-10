{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Grin.ExtendedSyntax.Datalog (calculateHPTResult) where

import Control.Monad (forM, forM_, void)
import Data.Int
import Language.Souffle.Interpreted as Souffle
import GHC.Generics
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Text (Text)
import Data.Functor.Foldable
import Control.Comonad (extract)
import Control.Comonad.Cofree
import Data.Either (fromRight)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe, fromJust)
import Data.Proxy
import Data.Bifunctor
import Data.List (sortBy)
import Data.Function (on)
import Grin.ExtendedSyntax.Pretty
import Debug.Trace
import System.FilePath (takeDirectory)

import qualified Data.Vector as Vector
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Grin.ExtendedSyntax.Syntax as Grin
import qualified Data.Text as Text
import qualified AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Result as Result

import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Pretty ()
import Paths_grin

{-
TODO:
[x] Handle As patterns
[x] Generate code that always have a single return value (In normalisation)
[x] Create HPTResult
[x] Add Datalog program to the resources
-}

data HPT = HPT

instance Souffle.Program HPT where
  type ProgramFacts HPT =
    '[ EntryPoint
     , External
     , ExternalParam
     , Move
     , LitAssign
     , Node
     , NodeArgument
     , Fetch
     , Store
     , Update
     , Call
     , CallArgument
     , NodePattern
     , NodeParameter
     , Case
     , Alt
     , AltLiteral
     , AltDefault
     , ReturnValue
     , FirstInst
     , NextInst

     , Heap
     , FunctionParameter
     , AltParameter
     , AbstractLocation
     , VariableAbstractLocation
     , VariableSimpleType
     , VariableNodeTag
     , VariableNodeParamType
     , FunParam
     , FunReturn
     ]
  programName = const "hpt"

type Function     = Text
type Boolean      = Int32
type SimpleType   = Text
type Number       = Int32
type Variable     = Text
type Literal      = Text
type Tag          = Text
type CodeName     = Text
type ExternalKind = Text
type Loc          = Text

mkBoolean :: Bool -> Boolean
mkBoolean = \case
  False -> 0
  True  -> 1

data EntryPoint         = EntryPoint !CodeName                                  deriving (Eq, Show, Generic)
data Move               = Move !Variable !Variable                              deriving (Eq, Show, Generic)
data LitAssign          = LitAssign !Variable !SimpleType !Literal              deriving (Eq, Show, Generic)
data Node               = Node !Variable !Tag                                   deriving (Eq, Show, Generic)
data NodeArgument       = NodeArgument !Variable !Number !Variable              deriving (Eq, Show, Generic)
data Fetch              = Fetch !Variable !Variable                             deriving (Eq, Show, Generic)
data Store              = Store !Variable !Variable                             deriving (Eq, Show, Generic)
data Update             = Update !Variable !Variable !Variable                  deriving (Eq, Show, Generic)
data Call               = Call !Variable !Function                              deriving (Eq, Show, Generic)
data CallArgument       = CallArgument !Variable !Number !Variable              deriving (Eq, Show, Generic)
data NodePattern        = NodePattern !Variable !Tag !Variable                  deriving (Eq, Show, Generic)
data NodeParameter      = NodeParameter !Variable !Number !Variable             deriving (Eq, Show, Generic)
data Case               = Case !Variable !Variable                              deriving (Eq, Show, Generic)
data Alt                = Alt !Variable !Variable !Tag                          deriving (Eq, Show, Generic)
data AltLiteral         = AltLiteral !Variable !Variable !SimpleType !Literal   deriving (Eq, Show, Generic)
data AltDefault         = AltDefault !Variable !Variable                        deriving (Eq, Show, Generic)
data ReturnValue        = ReturnValue !CodeName !Variable                       deriving (Eq, Show, Generic)
data FirstInst          = FirstInst !CodeName !Variable                         deriving (Eq, Show, Generic)
data NextInst           = NextInst !Variable !Variable                          deriving (Eq, Show, Generic)
data FunctionParameter  = FunctionParameter !Function !Number !Variable         deriving (Eq, Show, Generic)
data AltParameter       = AltParameter !Variable !Tag !Number !Variable         deriving (Eq, Show, Generic)
data External           = External !Function !Boolean !SimpleType !ExternalKind deriving (Eq, Show, Generic)
data ExternalParam      = ExternalParam !Function !Number !SimpleType           deriving (Eq, Show, Generic)

data Heap                     = Heap !Variable !Variable                                deriving (Eq, Show, Generic)
data AbstractLocation         = AbstractLocation !Loc                                   deriving (Eq, Show, Generic)
data VariableSimpleType       = VariableSimpleType !Variable !SimpleType                deriving (Eq, Show, Generic)
data VariableNodeTag          = VariableNodeTag !Variable !Tag                          deriving (Eq, Show, Generic)
data VariableNodeParamType    = VariableNodeParamType !Variable !Tag !Int32 !SimpleType deriving (Eq, Show, Generic)
data VariableAbstractLocation = VariableAbstractLocation !Variable !Loc                 deriving (Eq, Show, Generic)
data FunParam                 = FunParam !Function !Int32 !Variable                     deriving (Eq, Show, Generic)
data FunReturn                = FunReturn !Function !Variable                           deriving (Eq, Show, Generic)

instance Souffle.Marshal EntryPoint
instance Souffle.Marshal External
instance Souffle.Marshal ExternalParam
instance Souffle.Marshal Move
instance Souffle.Marshal LitAssign
instance Souffle.Marshal Node
instance Souffle.Marshal NodeArgument
instance Souffle.Marshal Fetch
instance Souffle.Marshal Store
instance Souffle.Marshal Update
instance Souffle.Marshal Call
instance Souffle.Marshal CallArgument
instance Souffle.Marshal NodePattern
instance Souffle.Marshal NodeParameter
instance Souffle.Marshal Case
instance Souffle.Marshal Alt
instance Souffle.Marshal AltLiteral
instance Souffle.Marshal AltDefault
instance Souffle.Marshal ReturnValue
instance Souffle.Marshal FirstInst
instance Souffle.Marshal NextInst
instance Souffle.Marshal FunctionParameter
instance Souffle.Marshal AltParameter

instance Souffle.Marshal Heap
instance Souffle.Marshal AbstractLocation
instance Souffle.Marshal VariableAbstractLocation
instance Souffle.Marshal VariableSimpleType
instance Souffle.Marshal VariableNodeTag
instance Souffle.Marshal VariableNodeParamType
instance Souffle.Marshal FunParam
instance Souffle.Marshal FunReturn

instance Souffle.Fact EntryPoint        where factName = const "EntryPoint"
instance Souffle.Fact External          where factName = const "External"
instance Souffle.Fact ExternalParam     where factName = const "ExternalParam"
instance Souffle.Fact Move              where factName = const "Move"
instance Souffle.Fact LitAssign         where factName = const "LitAssign"
instance Souffle.Fact Node              where factName = const "Node"
instance Souffle.Fact NodeArgument      where factName = const "NodeArgument"
instance Souffle.Fact Fetch             where factName = const "Fetch"
instance Souffle.Fact Store             where factName = const "Store"
instance Souffle.Fact Update            where factName = const "Update"
instance Souffle.Fact Call              where factName = const "Call"
instance Souffle.Fact CallArgument      where factName = const "CallArgument"
instance Souffle.Fact NodePattern       where factName = const "NodePattern"
instance Souffle.Fact NodeParameter     where factName = const "NodeParameter"
instance Souffle.Fact Case              where factName = const "Case"
instance Souffle.Fact Alt               where factName = const "Alt"
instance Souffle.Fact AltLiteral        where factName = const "AltLiteral"
instance Souffle.Fact AltDefault        where factName = const "AltDefault"
instance Souffle.Fact ReturnValue       where factName = const "ReturnValue"
instance Souffle.Fact FirstInst         where factName = const "FirstInst"
instance Souffle.Fact NextInst          where factName = const "NextInst"
instance Souffle.Fact FunctionParameter where factName = const "FunctionParameter"
instance Souffle.Fact AltParameter      where factName = const "AltParameter"

instance Souffle.Fact Heap                      where factName = const "Heap"
instance Souffle.Fact VariableSimpleType        where factName = const "VariableSimpleType"
instance Souffle.Fact AbstractLocation          where factName = const "AbstractLocation"
instance Souffle.Fact VariableAbstractLocation  where factName = const "VariableAbstractLocation"
instance Souffle.Fact VariableNodeTag           where factName = const "VariableNodeTag"
instance Souffle.Fact VariableNodeParamType     where factName = const "VariableNodeParamType"
instance Souffle.Fact FunParam                  where factName = const "FunParam"
instance Souffle.Fact FunReturn                 where factName = const "FunReturn"



calculateHPTResult :: Grin.Exp -> IO (Maybe Result.HPTResult)
calculateHPTResult exp = do

  -- The datalog program needs to be registered in the cabal file as data-file
  -- but the souffle-haskell library needs a directory to look for the program
  -- file. In the instance of the Program HPT the name must concide the one
  -- which is registered in the data-file part of the cabal file.
  hptProgramDirPath <- takeDirectory <$> getDataFileName "datalog/hpt/hpt.dl"

  let cfg = Souffle.Config hptProgramDirPath (Just "souffle")
  Souffle.runSouffleWith cfg $ do
    mprog <- Souffle.init HPT
    forM mprog $ \prog -> do
      Souffle.addFact prog $ EntryPoint "grinMain"
      para (structure prog) exp
      calcReturnValues prog exp
      nextInst prog exp
      Souffle.run prog
      r <- ResultData
        <$> Souffle.getFacts prog -- abstract location
        <*> Souffle.getFacts prog -- value abstract location
        <*> Souffle.getFacts prog -- variable simple type
        <*> Souffle.getFacts prog -- resultVariableNodeTag
        <*> Souffle.getFacts prog -- resultVariableNodeParamType
        <*> Souffle.getFacts prog -- resultFunReturn
        <*> Souffle.getFacts prog -- resultFunParam
        <*> Souffle.getFacts prog -- heap
      pure $ calcHPTResult r

structure :: Handle HPT -> Grin.ExpF (Grin.Exp, SouffleM ()) -> SouffleM ()
structure prog = \case
  Grin.ProgramF externals defs -> do
    convertExternals prog externals
    mapM_ snd defs

  -- f param0 param1 = ...
  -- .decl FunctionParameter(f:Function, i:number, parameter:Variable)
  Grin.DefF name args (_, body) -> do
    Souffle.addFacts prog $
      zipWith (\n a -> FunctionParameter (Grin.nameText name) n (Grin.nameText a)) [0..] args
    body

  -- result <- pure value
  -- .decl Move(result:Variable, value:Variable)
  Grin.EBindF (Grin.SReturn (Grin.Var val), lhs) (Grin.VarPat res) (_, rhs) -> do
    lhs
    Souffle.addFact prog $ Move (Grin.nameText res) (Grin.nameText val)
    rhs

  -- result <- pure 1
  -- .decl LitAssign(result:Variable, l:Literal)
  Grin.EBindF (Grin.SReturn (Grin.Lit l), lhs) (Grin.VarPat res) (_, rhs) -> do
    lhs
    Souffle.addFact prog $ uncurry (LitAssign (Grin.nameText res)) $ literalParams l
    rhs

  -- result_node <- pure (Ctag item0 item1)
  -- .decl Node(result_node:Variable, t:Tag)
  -- .decl NodeArgument(result_node:Variable, i:number, item:Variable)
  Grin.EBindF ((Grin.SReturn (Grin.ConstTagNode tag items)), lhs) (Grin.VarPat res) (_, rhs) -> do
    lhs
    Souffle.addFact prog $ Node (Grin.nameText res) (gtagToDtag tag)
    Souffle.addFacts prog $
      zipWith (\n v -> NodeArgument (Grin.nameText res) n (Grin.nameText v)) [0..] items
    rhs

  -- example: result <- fetch value
  -- .decl Fetch(result:Variable, value:Variable)
  Grin.EBindF (Grin.SFetch val, lhs) (Grin.VarPat res) (_, rhs) -> do
    lhs
    Souffle.addFact prog $ Fetch (Grin.nameText res) (Grin.nameText val)
    rhs

  -- example: result <- store value
  -- .decl Store(result:Variable, value:Variable)
  Grin.EBindF (Grin.SStore val, lhs) (Grin.VarPat res) (_, rhs) -> do
    lhs
    Souffle.addFact prog $ Store (Grin.nameText res) (Grin.nameText val)
    rhs

  -- example: result <- update target value
  -- .decl Update(result:Variable, target:Variable, value:Variable)
  Grin.EBindF (Grin.SUpdate target val, lhs) (Grin.VarPat res) (_, rhs) -> do
    lhs
    Souffle.addFact prog $ Update (Grin.nameText res) (Grin.nameText target) (Grin.nameText val)
    rhs

  -- call_result <- f value0 value1
  -- .decl Call(call_result:Variable, f:Function)
  -- .decl CallArgument(call_result:Variable, i:number, value:Variable)
  Grin.EBindF (Grin.SApp fun args, lhs) (Grin.VarPat res) (_, rhs) -> do
    lhs
    Souffle.addFact prog $ Call (Grin.nameText res) (Grin.nameText fun)
    Souffle.addFacts prog $
      zipWith (\n a -> CallArgument (Grin.nameText res) n (Grin.nameText a)) [0..] args
    rhs

  --  AsPat  { _bPatTag    :: Tag
  --         , _bPatFields :: [Name]
  --         , _bPatVar    :: Name
  --         }

  -- bind pattern
  -- node@(Ctag param0 param1) <- pure input_value
  -- .decl NodePattern(node:Variable, t:Tag, input_value:Variable)
  -- .decl NodeParameter(node:Variable, i:number, parameter:Variable)
  Grin.EBindF (Grin.SReturn (Grin.Var inp_val), lhs) (Grin.AsPat tag pms nd) (_, rhs) -> do
    lhs
    Souffle.addFact prog $ NodePattern (Grin.nameText nd) (gtagToDtag tag) (Grin.nameText inp_val)
    Souffle.addFacts prog $
      zipWith (\n p -> NodeParameter (Grin.nameText nd) n (Grin.nameText p)) [0..] pms
    rhs

  -- case + alt
  -- example:
  -- case_result <- case scrut of
  --   alt_value@(Ctag param0 param1) -> basic_block_name arg0 arg1
  -- .decl Case(case_result:Variable, scrutinee:Variable)
  -- .decl Alt(case_result:Variable, alt_value:Variable, t:Tag)
  -- .decl AltParameter(case_result:Variable, t:Tag, i:number, parameter:Variable)
  -- .decl AltLiteral(case_result:Variable, alt_value:Variable, l:Literal)
  -- .decl AltDefault(case_result:Variable, alt_value :: Variable)
  Grin.EBindF (Grin.ECase scr alts, lhs) (Grin.VarPat cs_res) (_, rhs) -> do
    lhs
    Souffle.addFact prog $ Case (Grin.nameText cs_res) (Grin.nameText scr)
    -- -- TODO: Improve performance by grouping alternatives
    forM_ alts $ \case
      Grin.Alt (Grin.NodePat tag args) n _ -> do
        Souffle.addFact prog $ Alt (Grin.nameText cs_res) (Grin.nameText n) (gtagToDtag tag)
        Souffle.addFacts prog $
          zipWith
            (\j a -> AltParameter (Grin.nameText cs_res) (gtagToDtag tag) j (Grin.nameText a))
            [0..]
            args

      -- TODO: Handle literals better
      Grin.Alt (Grin.LitPat l) n _ -> do
        let (st, lt) = literalParams l
        Souffle.addFact prog $ AltLiteral (Grin.nameText cs_res) (Grin.nameText n) st lt

      Grin.Alt Grin.DefaultPat n _ -> do
        Souffle.addFact prog $ AltDefault (Grin.nameText cs_res) (Grin.nameText n)
    rhs

  other -> void $ traverse snd other

-- * Return values

-- TODO: Make this monadic
calcReturnValues :: Handle HPT -> Grin.Exp -> SouffleM ()
calcReturnValues prog = snd . histo (returnValueAlg prog)

returnValueAlg
  :: Handle HPT
  -> Grin.ExpF (Cofree Grin.ExpF (Maybe Grin.Name, SouffleM ()))
  -> (Maybe Grin.Name, SouffleM ())
returnValueAlg prog = \case
  Grin.SReturnF (Grin.Var name) -> (Just name, pure ())
  Grin.SReturnF val             -> (Nothing, pure ())
  Grin.EBindF ((_, lhs) :< Grin.ECaseF _ alts) (Grin.VarPat v) (extract -> (returnValue, rhs))
    -> let altReturnValues = mapMaybe (fst . extract) alts
       in ( returnValue
          , do lhs
               Souffle.addFacts prog
                $ map (\r -> NextInst (Grin.nameText r) (Grin.nameText v)) altReturnValues
               rhs
          )
  Grin.EBindF (extract -> (_, lhs)) _ (extract -> (returnValue, rhs))
    -> (returnValue, lhs >> rhs)
  Grin.AltF _ codeName (extract -> (Just returnValue, body)) ->
    ( Just returnValue
    , do Souffle.addFact prog $ ReturnValue (Grin.nameText codeName) (Grin.nameText returnValue)
         body
    )
  Grin.DefF codeName _ (extract -> (Just returnValue, body)) ->
    ( Nothing
    , do Souffle.addFact prog $ ReturnValue (Grin.nameText codeName) (Grin.nameText returnValue)
         body
    )
  rest ->
    ( Nothing
    , void $ traverse (snd . extract) rest
    )

nextInst :: Handle HPT -> Grin.Exp -> SouffleM ()
nextInst prog = void . para (nextInstAlg prog)

-- | The next instrument makes a chain of variable associations, between binds
nextInstAlg :: Handle HPT -> Grin.ExpF (Grin.Exp, SouffleM (Maybe Grin.Name)) -> SouffleM (Maybe Grin.Name)
nextInstAlg prog = \case
  Grin.DefF codeName _ (Grin.EBind _ (Grin.VarPat v) _, body) -> do
    Souffle.addFact prog $ FirstInst (Grin.nameText codeName) (Grin.nameText v)
    void body
    pure Nothing
  Grin.AltF _ n (Grin.EBind _ (Grin.VarPat v) _, body) -> do
    Souffle.addFact prog $ NextInst (Grin.nameText n) (Grin.nameText v)
    void body
    pure $ Just n
  Grin.ECaseF v alts -> do
    nis <- catMaybes <$> mapM snd alts
    Souffle.addFacts prog $
      map (\ni -> NextInst (Grin.nameText v) (Grin.nameText ni)) nis
    pure Nothing
  Grin.EBindF (elhs, lhs) (Grin.VarPat v) (_, rhs) -> do
    void lhs
    mni <- rhs
    forM_ mni $ \ni -> do
      Souffle.addFact prog $ NextInst (Grin.nameText v) (Grin.nameText ni)
    pure $ case elhs of
      Grin.ECase{} -> Nothing
      _            -> Just v
  rest -> do
    mapM_ snd rest
    pure Nothing

-- * Externals

convertExternals :: Handle HPT -> [Grin.External] -> SouffleM ()
convertExternals prog externals = forM_ externals $
  \(Grin.External n rt pts e k) -> do
      Souffle.addFact prog $
        External (Grin.nameText n) (mkBoolean e) (asDatalogSimpleType rt) (externalKind k)
      Souffle.addFacts prog $
        zipWith
          (\j t -> ExternalParam (Grin.nameText n) j (asDatalogSimpleType t))
          [0..]
          pts
  where
    asDatalogSimpleType :: Grin.Ty -> SimpleType
    asDatalogSimpleType = \case
      Grin.TySimple st -> stToDatalogST st
      _ -> error "asDatalogSimpleType: None handled"

externalKind :: Grin.ExternalKind -> ExternalKind
externalKind = \case
  Grin.PrimOp -> "primop"
  Grin.FFI    -> "ffi"

-- * Convert to HPTResult

data ResultData = ResultData
  { resultAbstractLocations     :: [AbstractLocation]
  , resultValueAbstractLocation :: [VariableAbstractLocation]
  , resultVariableSimpleType    :: [VariableSimpleType]
  , resultVariableNodeTag       :: [VariableNodeTag]
  , resultVariableNodeParamType :: [VariableNodeParamType]
  , resultFunReturn             :: [FunReturn]
  , resultFunParam              :: [FunParam]
  , resultHeap                  :: [Heap]
  } deriving (Eq, Show)

abstractLocationMap :: [AbstractLocation] -> (Map.Map Text Int, Map.Map Int Text)
abstractLocationMap as = (ti, it)
  where
    (ti,it) = bimap Map.fromList Map.fromList
            $ unzip
            $ zipWith (\(AbstractLocation l) n -> ((l,n), (n,l))) as [0..]

variableNodeMap
  :: [VariableNodeTag]
  -> [VariableNodeParamType]
  -> Map.Map Text [(Tag, [Set.Set SimpleType])]
variableNodeMap ns ps = Map.unionsWith (++)
  [ Map.singleton n [(t,ts)]
  | VariableNodeTag n t <- ns
  , let ps0 = Map.unionsWith mappend
            $ mapMaybe (\(VariableNodeParamType n0 t0 i s)
                -> if n == n0 && t == t0
                    then Just $ Map.singleton i (Set.singleton s)
                    else Nothing) ps
  , let ts = case unzip $ Map.toList ps0 of
              (as, es) | as == [0 .. fromIntegral (length as - 1)] -> es
              (_, es) -> error $ "in positions: " ++ show ps0 -- TODO
  ]

functionNameMap
  :: [FunParam]
  -> [FunReturn]
  -> Map.Map Grin.Name Result.TypeSet
  -> Map.Map Grin.Name (Result.TypeSet, Vector.Vector Result.TypeSet)
functionNameMap ps rs vars = Map.fromList
  [ (Grin.mkName fun, (ret, params))
  | FunReturn fun ret0 <- rs
  , let ps0 = Map.unionsWith mappend
            $ mapMaybe
                (\(FunParam f i p)
                  -> if f == fun
                      then Map.singleton i <$> Map.lookup (Grin.mkName p) vars
                      else Nothing)
                ps
  , let params = case unzip $ Map.toList ps0 of
          (as,es) | as == [0 .. fromIntegral (length as - 1)] -> Vector.fromList es
          (_, es) -> error $ "functionMap: in positions: " ++ show (fun, ret0, ps0) -- TODO
  , let ret = fromMaybe (error $ "functionMap:" ++ show (ret0, vars))
            $ Map.lookup (Grin.mkName ret0) vars
  ]

heapMap
  :: [Heap]
  -> Map.Map Text Int
  -> Map.Map Grin.Name Result.TypeSet
  -> Vector.Vector Result.NodeSet
heapMap hs nameToLoc vars
  = Vector.generate
      (Map.size nameToLoc)
      (\l -> fromMaybe (error $ "heapMap #0: " ++ show (l, heapVals)) $ Map.lookup l heapVals) -- TODO: mempty
  where
    heapVals = Map.unionsWith mappend
      [ Map.singleton loc nodeSet
      | Heap ln t <- hs
      -- TODO: Fix below, we shouldn't avoid use pattern match errors.
      , let loc = fromMaybe (error $ "heapMap #1: " ++ show (ln, nameToLoc))
                $ Map.lookup ln nameToLoc
      , let (Result.TypeSet _ nodeSet)
              = fromMaybe (error $ "heapMap #2: " ++ show (t,vars))
              $ Map.lookup (Grin.mkName t) vars
      ]

calcHPTResult :: ResultData -> Result.HPTResult
calcHPTResult (ResultData{..}) = Result.HPTResult memory register function
  where
    (nameToLoc, locToName) = abstractLocationMap resultAbstractLocations
    nodeTags = Map.toList $ variableNodeMap resultVariableNodeTag resultVariableNodeParamType
    memory = heapMap resultHeap nameToLoc register
    register = Map.unionsWith (<>)
               ( map (\(VariableSimpleType n t)
                      -> Map.singleton (Grin.mkName n)
                          (toTypeSet $ either (error . show) id $ datalogStToSt t))
                     resultVariableSimpleType
                 ++
                 map (\(VariableAbstractLocation n l)
                      -> case Map.lookup n nameToLoc of
                          Nothing -> mempty
                          Just l  -> Map.singleton
                            (Grin.mkName n)
                            (Result.TypeSet
                              (Set.singleton (Result.T_Location l))
                              mempty))
                     resultValueAbstractLocation
                 ++
                 concatMap
                  (\(n, ps)
                    -> map (\(tag, types)
                        -> Map.singleton
                            (Grin.mkName n)
                            (Result.TypeSet mempty
                              (Result.NodeSet (Map.singleton
                                (dtagToGtag tag)
                                (Vector.fromList $ map
                                  (Set.map
                                    (either
                                      (Result.T_Location . fromJust . flip Map.lookup nameToLoc) -- TODO: mempty
                                      id . datalogStToRSt))
                                  types))))
                    ) ps)
                  nodeTags
               )

    function = functionNameMap resultFunParam resultFunReturn register

class ToTypeSet t where
  toTypeSet :: t -> Result.TypeSet

instance ToTypeSet Grin.SimpleType where
  toTypeSet = \case
    Grin.T_Int64   -> Result.TypeSet (Set.singleton Result.T_Int64) mempty
    Grin.T_Word64  -> Result.TypeSet (Set.singleton Result.T_Word64) mempty
    Grin.T_Float   -> Result.TypeSet (Set.singleton Result.T_Float) mempty
    Grin.T_Bool    -> Result.TypeSet (Set.singleton Result.T_Bool) mempty
    Grin.T_Char    -> Result.TypeSet (Set.singleton Result.T_Char) mempty
    Grin.T_Unit    -> Result.TypeSet (Set.singleton Result.T_Unit) mempty
    Grin.T_String  -> Result.TypeSet (Set.singleton Result.T_String) mempty

-- * Helpers

gtagToDtag :: Grin.Tag -> Tag
gtagToDtag (Grin.Tag tt name) = (renderTagType tt) <> Grin.nameText name
  where
    renderTagType :: Grin.TagType -> Text
    renderTagType Grin.C      = "C"
    renderTagType Grin.F      = "F"
    renderTagType (Grin.P m)  = "P-" <> Text.pack (show m) <> "-"

dtagToGtag :: Tag -> Grin.Tag
dtagToGtag tag = case Text.unpack tag of
  'C':name -> Grin.Tag Grin.C (Grin.mkName $ Text.pack name)
  'F':name -> Grin.Tag Grin.F (Grin.mkName $ Text.pack name)
  'P':rest -> case Text.splitOn "-" $ Text.pack rest of
    ["P",(read . show) -> m, name] -> Grin.Tag (Grin.P m) (Grin.NM name)
    _ -> error $ show tag

literalParams :: Grin.Lit -> (SimpleType, Literal)
literalParams sv = case sv of
  Grin.LInt64  i -> (stToDatalogST Grin.T_Int64,  Text.pack $ show i)
  Grin.LWord64 w -> (stToDatalogST Grin.T_Word64, Text.pack $ show w)
  Grin.LFloat  f -> (stToDatalogST Grin.T_Float,  Text.pack $ show f)
  Grin.LBool   b -> (stToDatalogST Grin.T_Bool,   Text.pack $ show b)
  Grin.LChar   c -> (stToDatalogST Grin.T_Char,   Text.pack $ show c)
  Grin.LString s -> (stToDatalogST Grin.T_String, Text.pack $ show s) -- TODO

stToDatalogST :: Grin.SimpleType -> SimpleType
stToDatalogST = \case
  Grin.T_Int64   -> "Int64"
  Grin.T_Word64  -> "Word64"
  Grin.T_Float   -> "Float"
  Grin.T_Bool    -> "Bool"
  Grin.T_Char    -> "Char"
  Grin.T_Unit    -> "Unit"
  Grin.T_String  -> "String"
  other -> error $ "stToDatalogST: None handled case: " ++ show other

datalogStToSt :: SimpleType -> (Either Text Grin.SimpleType)
datalogStToSt = \case
  "Int64"  -> Right $ Grin.T_Int64
  "Word64" -> Right $ Grin.T_Word64
  "Float"  -> Right $ Grin.T_Float
  "Bool"   -> Right $ Grin.T_Bool
  "Char"   -> Right $ Grin.T_Char
  "Unit"   -> Right $ Grin.T_Unit
  "String" -> Right $ Grin.T_String
  other    -> Left other

datalogStToRSt :: SimpleType -> (Either Text Result.SimpleType)
datalogStToRSt = \case
  "Int64"  -> Right $ Result.T_Int64
  "Word64" -> Right $ Result.T_Word64
  "Float"  -> Right $ Result.T_Float
  "Bool"   -> Right $ Result.T_Bool
  "Char"   -> Right $ Result.T_Char
  "Unit"   -> Right $ Result.T_Unit
  "String" -> Right $ Result.T_String
  other    -> Left other
