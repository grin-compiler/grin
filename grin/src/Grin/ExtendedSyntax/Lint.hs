{-# LANGUAGE ViewPatterns, LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Grin.ExtendedSyntax.Lint
  ( lint
  , allWarnings
  , noDDEWarnings
  , Error(..)
  ) where

import Text.Printf

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Control.Comonad.Cofree
import Control.Monad.State
import Control.Monad.Writer hiding (Alt)
import qualified Control.Comonad.Trans.Cofree as CCTC
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (findIndices)
import Lens.Micro.Platform
import Lens.Micro.Extra
import Text.PrettyPrint.ANSI.Leijen (Pretty, plain)
import Data.String
import Control.Comonad (extract)
import qualified Data.Vector as Vector
import Control.Applicative (liftA2)
import Data.Functor.Infix ((<$$>))

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.TypeEnv hiding (typeOfVal)
import Transformations.ExtendedSyntax.Util

import Debug.Trace
import Data.Maybe

-- TODO: remove redundant syntaxE s

{-
Linter is responsible for the semantical checks of the program.
-}

{-
  - AST shape (syntax)
    done - exp
    - val
  - recognise primitive functions
  - scope checking ; requires monad (full traversal)
  - type checking (using the type env)
  - pattern checking
  - node item checking (no node in node)
  - case:
    - overlapping alternatives
    - uncovered cases
    - fully saturated alternatives
  - fully saturated node creation eg (CPair 1 2) vs (CPair 1)
-}

{-
  question:
    how to show errors?
      - annotate expressions with the error using cofree
      - annotate expressionf with id using cofree, then build error map referencing to expressions
-}

data Error = Error
  { before  :: Bool
  , message :: String
  }

msg :: String -> Error
msg = Error False

beforeMsg :: String -> Error
beforeMsg = Error True

data ExpCtx
  = ProgramCtx
  | DefCtx
  | ExpCtx
  | SimpleExpCtx
  | SEWithoutNodesCtx
  | AltCtx
  deriving Eq

data ValCtx
  = ValCtx
  | SimpleValCtx
  deriving Eq

showExpCtx :: ExpCtx -> String
showExpCtx = \case
  ProgramCtx        -> "Program"
  DefCtx            -> "Def"
  ExpCtx            -> "Exp"
  SimpleExpCtx      -> "SimpleExp"
  SEWithoutNodesCtx -> "SimpleExp without nodes"
  AltCtx            -> "Alt"

showValCtx :: ValCtx -> String
showValCtx = \case
  ValCtx        -> "Val"
  SimpleValCtx  -> "SimpleVal"

data Env
  = Env
  { envNextId       :: Int
  , envVars         :: Map Name Int -- exp id
  , envErrors       :: Map Int [Error]
  , envDefinedNames :: Map Name DefRole
  , envFunArity     :: Map Name Int
  , envWarningKinds :: [WarningKind] -- Allowed warning kinds
  }

emptyEnv = Env
  { envNextId       = 0
  , envVars         = mempty
  , envErrors       = mempty
  , envDefinedNames = mempty
  , envFunArity     = mempty
  , envWarningKinds = []
  }

type Lint   = State Env
type Check  = WriterT [Error] Lint
type TypedExp = Cofree ExpF (Maybe Type)

expId :: Lint Int
expId = gets envNextId

nextId :: Lint ()
nextId = modify' $ \env@Env{..} -> env {envNextId = succ envNextId}

{-
  TODO:
    type check
-}

data WarningKind
  = Syntax
  | Semantics
  | DDE
  deriving (Enum, Eq, Ord, Show)

allWarnings :: [WarningKind]
allWarnings = [Syntax .. DDE]

noDDEWarnings :: [WarningKind]
noDDEWarnings = [Syntax, Semantics]

warning :: WarningKind -> [Error] -> Check ()
warning w m = do
  ws <- gets envWarningKinds
  when (w `elem` ws) $
    tell m

syntaxVal :: ValCtx -> Val -> Check Bool
syntaxVal ctx = \case
  Lit{} -> pure True
  Var{} -> pure True

  Undefined (T_NodeSet{})
    | ctx == ValCtx -> pure True

  _ | ctx == ValCtx -> pure True

  _ -> warning Syntax [msg $ "Syntax error - expected " ++ showValCtx ctx] >> pure False

syntaxVal_ :: ValCtx -> Val -> Check ()
syntaxVal_ = void <$$> syntaxVal

{-
  ConstTagNode  Tag  [SimpleVal] -- complete node (constant tag) ; HIGH level GRIN
  VarTagNode    Name [SimpleVal] -- complete node (variable tag)
  ValTag        Tag
  Unit                           -- HIGH level GRIN
  Lit Lit                        -- HIGH level GRIN
  Var Name                       -- HIGH level GRIN
-}

syntaxExp :: ExpCtx -> ExpCtx -> Check ()
syntaxExp expected given | given == expected = pure ()
syntaxExp ExpCtx SimpleExpCtx = pure ()
syntaxExp SimpleExpCtx SEWithoutNodesCtx = pure ()
syntaxExp ExpCtx SEWithoutNodesCtx = pure ()
syntaxExp expected _ = warning Syntax [msg $ "Syntax error - expected " ++ showExpCtx expected]

checkNameDef :: DefRole -> Name -> Check ()
checkNameDef role name = do
  defined <- state $ \env@Env{..} ->
    ( Map.member name envDefinedNames
    , env {envDefinedNames = Map.insert name role envDefinedNames}
    )
  when defined $ do
    warning Semantics [msg $ printf "multiple defintion of %s" name]

    -- TODO: state -> gets
checkNameUse :: Name -> Check ()
checkNameUse name = do
  defined <- state $ \env@Env{..} -> (Map.member name envDefinedNames, env)
  unless defined $ do
    warning Syntax [msg $ printf "undefined variable: %s" name]

checkVarScopeM :: ExpF a -> Check ()
checkVarScopeM exp = do
  case exp of
    DefF _ _ _ -> pure () -- Function definitions are already registered
    _          -> mapM_ (uncurry checkNameDef) $ foldNameDefExpF (\r n -> [(r,n)]) exp
  mapM_ checkNameUse $ foldNameUseExpF (:[]) exp

plainShow :: (Pretty p) => p -> String
plainShow = show . plain . pretty

unionSType :: SimpleType -> SimpleType -> Maybe SimpleType
unionSType (T_Location l1) (T_Location l2) = Just $ T_Location (l1 ++ l2)
unionSType T_Dead t = Just t
unionSType t T_Dead = Just t
unionSType t1 t2
  | t1 == t2 = Just t1
  | otherwise = Nothing

unionType :: Type -> Type -> Maybe Type
unionType (T_SimpleType t1) (T_SimpleType t2) = T_SimpleType <$> unionSType t1 t2
unionType (T_NodeSet ns1) (T_NodeSet ns2) =
  fmap T_NodeSet
  $ sequenceA
  $ Map.map (fmap Vector.fromList . sequenceA)
  $ Map.unionWith (zipWith (join <$$> liftA2 unionSType)) (Map.map (map Just . Vector.toList) ns1) (Map.map (map Just . Vector.toList) ns2)
unionType _ _ = Nothing

annotate :: TypeEnv -> Exp -> TypedExp
annotate te = cata builder where
  builder :: ExpF TypedExp -> TypedExp
  builder = \case
    ProgramF exts defs -> Nothing :< ProgramF exts defs
    DefF n ps body -> (te ^? function . at n . _Just . _1) :< DefF n ps body
    SReturnF val -> mTypeOfValTE te val :< SReturnF val
    SStoreF val -> Nothing :< SStoreF val -- Store returns a location type that is associated in its binded variable
    SUpdateF name val -> Just unit_t :< SUpdateF name val
    SFetchF var ->
      (do locs <- mTypeOfValTE te (Var var) ^? _Just . _T_SimpleType . _T_Location
          let (n:ns) = catMaybes $ map (\l -> te ^? location . at l . _Just . to T_NodeSet) locs
          foldM unionType n ns
      )
      :< SFetchF var -- Fetch returns a value based on its arguments that is associated in its binded variable
    SAppF name params -> (te ^? function . at name . _Just . _1) :< SAppF name params
    AltF cpat n body -> extract body :< AltF cpat n body
    ECaseF var alts ->
      (do case catMaybes $ map extract alts of
            [] -> Nothing
            (t:ts) -> foldM unionType t ts)
      :< ECaseF var alts
    EBindF lhs pat rhs -> extract rhs :< EBindF lhs pat rhs
    SBlockF body -> extract body :< SBlockF body

noAnnotation :: Exp -> TypedExp
noAnnotation = cata (Nothing :<)

check :: ExpF (ExpCtx, TypedExp) -> Check () -> Lint (CCTC.CofreeF ExpF Int (ExpCtx, TypedExp))
check exp nodeCheckM = do
  idx <- expId
  errors <- execWriterT (nodeCheckM >> checkVarScopeM exp)
  unless (null errors) $ do
    modify' $ \env@Env{..} -> env {envErrors = Map.insert idx errors envErrors}
  nextId
  pure (idx CCTC.:< exp )

lint :: [WarningKind] -> Maybe TypeEnv -> Exp -> (Cofree ExpF Int, Map Int [Error])
lint warningKinds mTypeEnv exp@(Program exts _) =
  fmap envErrors $ flip runState (emptyEnv { envWarningKinds = warningKinds }) $ do
    forM_ exts $ \External{..} -> do
      modify' $ \env@Env{..} -> env { envDefinedNames = Map.insert eName FunName envDefinedNames }
    cata functionNames exp
    anaM builder (ProgramCtx, maybe noAnnotation annotate mTypeEnv exp)
  where
  functionNames :: ExpF (Lint ()) -> Lint ()
  functionNames = \case
    ProgramF exts defs -> sequence_ defs
    DefF name args body -> do
      modify' $ \env@Env{..} -> env
        { envDefinedNames = Map.insert name FunName envDefinedNames
        , envFunArity = Map.insert name (length args) envFunArity
        }
      forM_ args $ \p -> modify' $ \env@Env{..} -> env { envDefinedNames = Map.insert p FunParam envDefinedNames }
      body
    rest -> pure ()

  builder :: (ExpCtx, TypedExp) -> Lint (CCTC.CofreeF ExpF Int (ExpCtx, TypedExp))
  builder (ctx, e) = case e of

    (_ :< ProgramF{}) -> checkWithChild DefCtx $ do
      syntaxE ProgramCtx

    (_ :< DefF name args _) -> checkWithChild ExpCtx $ do
      syntaxE DefCtx

    -- Exp
    -- The result Fetch should be bound to a variable to make DDE simpler
    (_ :< EBindF leftExp bPat rightExp) -> do
      -- TODO: is this really needed?
      let lhsCtx = if isn't _OnlyVarPat bPat then SEWithoutNodesCtx else SimpleExpCtx
      check (EBindF (lhsCtx, leftExp) bPat (ExpCtx, rightExp)) $ do
        syntaxE ExpCtx
        -- QUESTION: Is this needed wit hthe new syntax? Undefined introduction is still an open question.
        when (isFetchF leftExp && isn't _OnlyVarPat bPat) (warning DDE [msg $ "The result of Fetch can only be bound to a variable: " ++ plainShow bPat])

        when (isn't _OnlyVarPat bPat) $ do
          forM_ mTypeEnv $ \typeEnv -> do
            fromMaybe (pure ()) $ case bPat of
              AsPat tag fields v -> do -- Maybe
                expectedPatType <- normalizeType <$> mTypeOfValTE typeEnv (ConstTagNode tag fields)
                lhsType         <- normalizeType <$> extract leftExp
                pure $ do -- Lint
                  -- NOTE: This can still give false positive errors, because bottom-up typing can only approximate the result of HPT.
                  when (sameType expectedPatType lhsType == Just False) $ do
                    warning Semantics $ [beforeMsg $ unwords
                      ["Invalid pattern match for", plainShow bPat ++ "." , "Expected pattern of type:", plainShow expectedPatType ++ ",", "but got:", plainShow lhsType]]

    (_ :< ECaseF scrut alts0) -> checkWithChild AltCtx $ do
      syntaxE SEWithoutNodesCtx
      let alts = getF <$> alts0
      -- Overlapping node alternatives
      let tagOccurences =
            Map.unionsWith (+) $
            map (`Map.singleton` 1) $
            concatMap (^.. _AltFCPat . _CPatNodeTag) alts
      forM_ (Map.keys $ Map.filter (>1) tagOccurences) $ \(tag :: Tag) ->
        warning Semantics [beforeMsg $ printf "case has overlapping node alternatives %s" (plainShow tag)]
      -- Overlapping literal alternatives
      let literalOccurences =
            Map.unionsWith (+) $
            map (`Map.singleton` 1) $
            concatMap (^.. _AltFCPat . _CPatLit) alts
      forM_ (Map.keys $ Map.filter (>1) literalOccurences) $ \(lit :: Lit) ->
        warning Semantics [beforeMsg $ printf "case has overlapping literal alternatives %s" (plainShow lit)]

      let noOfDefaults = length $ findIndices (has (_AltFCPat . _CPatDefault)) alts
      -- More than one default
      when (noOfDefaults > 1) $ do
        warning Semantics [beforeMsg $ "case has more than one default alternatives"]

      forM_ mTypeEnv $ \typeEnv -> do
        -- Case variable has a location type
        let mSt = typeEnv ^? variable . at scrut . _Just . _T_SimpleType
        case mSt of
          Just st
            | has _T_Location st || has _T_String st || has _T_Float st
            -> warning Semantics [beforeMsg $ printf "case variable %s has non-supported pattern match type: %s" scrut (plainShow st)]
          _ -> pure () -- TODO

        -- Non-covered alternatives
        when (noOfDefaults == 0) $ do
          let mTags = typeEnv ^? variable . at scrut . _Just . _T_NodeSet . to Map.keys
          case mTags of
            Just tags -> do
              forM_ tags $ \tag -> when (Map.notMember tag tagOccurences) $ do
                warning Semantics [beforeMsg $ printf "case has non-covered alternative %s" (plainShow tag)]
            _ -> pure () -- TODO

    -- Simple Exp
    (_ :< SAppF name args) -> checkWithChild ctx $ do
      syntaxE SEWithoutNodesCtx
      -- Test existence of the function.
      Env{..} <- get
      when (not $ isExternalName exts name) $
        case Map.lookup name envDefinedNames of
          (Just FunName) -> pure ()
          (Just _)       -> warning Syntax [msg $ printf "non-function in function call: %s" name]
          Nothing        -> warning Syntax [msg $ printf "non-defined function is called: %s" name]
      -- Non saturated function call
      forM_ (Map.lookup name envFunArity) $ \n -> when (n /= length args) $ do
        warning Syntax [msg $ printf "non-saturated function call: %s" name]

    -- Only simple values should be returned,
    -- unless the returned value is bound to a variable.
    -- In that case the Return node is a left-hand side of a binding,
    -- which means it is inside a SimpleExpCtx.
    -- This is becuase only binding left-hand sides can be in SimpleExpCtx.
    (_ :< SReturnF val) -> checkWithChild ctx $ do
      (onlySimpleVals,errs) <- censorListen $ syntaxVal SimpleValCtx val
      let hasNoNodes = val == Unit || onlySimpleVals
      case ctx of
        -- last expression in a binding sequence or a single, standalone expression
        ExpCtx | hasNoNodes -> syntaxE SEWithoutNodesCtx
               | otherwise  -> warning DDE [msg $ "Last return expressions can only return non-node values: " ++ plainShow (SReturn val)]
        -- lhs of a binding
        SimpleExpCtx | hasNoNodes -> syntaxE SEWithoutNodesCtx
        -- lhs of a bidning where the LPat is not a variable
        SEWithoutNodesCtx | hasNoNodes -> syntaxE SEWithoutNodesCtx

        _ -> syntaxE SimpleExpCtx

    (_ :< SStoreF arg) -> checkWithChild ctx $ do
      syntaxE SEWithoutNodesCtx
      forM_ mTypeEnv $ \typeEnv -> do
        -- Store has given a primitive type
        let mTags = typeEnv ^? variable . at arg . _Just . _T_NodeSet . to Map.keys
            mSt   = typeEnv ^? variable . at arg . _Just . _T_SimpleType
        case (mTags,mSt) of
          (Just tags,_) -> pure ()
          (_,  Just st) | st /= T_Dead ->
             warning Semantics [msg $ printf "store has given a primitive value: %s :: %s" (plainShow arg) (plainShow st)]
          _ -> pure ()

    (_ :< SFetchF name) -> checkWithChild ctx $ do
      syntaxE SEWithoutNodesCtx
      -- Non location parameter for fetch
      forM_ mTypeEnv $ \typeEnv -> if
        | Just _ <- typeEnv ^? variable . at name . _Just . _T_SimpleType . _T_Location
          -> pure ()
        | Just st <- typeEnv ^? variable . at name . _Just . _T_SimpleType
          -> when (st /= T_Dead) $ warning Semantics [msg $ printf "the parameter of fetch is a primitive type: %s :: %s" (plainShow name) (plainShow st)]
        | Just ns <- typeEnv ^? variable . at name . _Just . _T_NodeSet
          -> warning Semantics [msg $ printf "the parameter of fetch is a node type: %s" (plainShow name)]
        | otherwise -> pure ()

    (_ :< SUpdateF name arg) -> checkWithChild ctx $ do
      syntaxE SEWithoutNodesCtx

      -- Non location parameter for update
      forM_ mTypeEnv $ \typeEnv -> if
        | Just _ <- typeEnv ^? variable . at name . _Just . _T_SimpleType . _T_Location
          -> pure ()
        | Just st <- typeEnv ^? variable . at name . _Just . _T_SimpleType
          -> when (st /= T_Dead) $ warning Semantics [msg $ printf "the parameter of update is a primitive type: %s :: %s" (plainShow name) (plainShow st)]
        | Just ns <- typeEnv ^? variable . at name . _Just . _T_NodeSet
          -> warning Semantics [msg $ printf "the parameter of update is a node type: %s" (plainShow name)]
        | otherwise -> pure ()

      forM_ mTypeEnv $ \typeEnv -> do
        -- Update has given a primitive type
        let mTags = typeEnv ^? variable . at arg . _Just . _T_NodeSet . to Map.keys
            mSt   = typeEnv ^? variable . at arg . _Just . _T_SimpleType
        case (mTags,mSt) of
          (Just tags,_) -> pure ()
          (_,  Just st) | st /= T_Dead ->
            warning Semantics [msg $ printf "update has given a primitive value: %s :: %s" (plainShow arg) (plainShow st)]
          _ -> pure ()

    (_ :< SBlockF{}) -> checkWithChild ExpCtx $ do
      syntaxE SEWithoutNodesCtx

    -- Alt
    -- TODO: Define some checks for the alt name.
    -- For example, that it is a fresh variable.
    (_ :< AltF cpat n _) -> checkWithChild ExpCtx $ do
      syntaxE AltCtx

    where
      syntaxE = syntaxExp ctx
      checkWithChild childCtx m = check ((childCtx,) <$> (getF e)) m
      getF (_ :< f) = f

      isFetchF (getF -> SFetchF{}) = True
      isFetchF _ = False

      -- Collects the side-effects without appending it to the output.
      censorListen :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m (a,w)
      censorListen = censor (const mempty) . listen
