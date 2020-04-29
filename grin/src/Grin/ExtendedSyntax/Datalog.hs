{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Grin.ExtendedSyntax.Datalog where

import Control.Monad (forM_)
import Data.Int
import Language.Souffle.Interpreted as Souffle
import GHC.Generics
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Text (Text)
import qualified Grin.ExtendedSyntax.Syntax as Grin
import qualified Data.Text as Text

data HPT = HPT

instance Souffle.Program HPT where
  type ProgramFacts HPT =
    '[ External
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
     , FunctionParameter
     , AltParameter
     ]
  programName = const "hpt"

type Function   = Text
type Boolean    = Int32
type SimpleType = Text
type Number     = Int32
type Variable   = Text
type Literal    = Text
type Tag        = Text -- TODO: TagType?
type CodeName   = Text

--instance Souffle.Marshal Grin.Name where
--  push (Grin.NM n) = push n
--  pop = Grin.NM <$> pop

data External           = External !Function !Boolean !SimpleType               deriving (Eq, Show, Generic)
data ExternalParam      = ExternalParam !Function !Number !SimpleType           deriving (Eq, Show, Generic)
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
data AltLiteral         = AltLiteral !Variable !Variable !Literal               deriving (Eq, Show, Generic)
data AltDefault         = AltDefault !Variable !Variable                        deriving (Eq, Show, Generic)
data ReturnValue        = ReturnValue !CodeName !Variable                       deriving (Eq, Show, Generic)
data FirstInst          = FirstInst !CodeName !Variable                         deriving (Eq, Show, Generic)
data NextInst           = NextInst !Variable !Variable                          deriving (Eq, Show, Generic)
data FunctionParameter  = FunctionParameter !Function !Number !Variable         deriving (Eq, Show, Generic)
data AltParameter       = AltParameter !Variable !Tag !Number {-!Variable-}         deriving (Eq, Show, Generic)

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

instance Souffle.Fact External          where factName = const "external"
instance Souffle.Fact ExternalParam     where factName = const "externalparam"
instance Souffle.Fact Move              where factName = const "move"
instance Souffle.Fact LitAssign         where factName = const "litassign"
instance Souffle.Fact Node              where factName = const "node"
instance Souffle.Fact NodeArgument      where factName = const "nodeargument"
instance Souffle.Fact Fetch             where factName = const "fetch"
instance Souffle.Fact Store             where factName = const "store"
instance Souffle.Fact Update            where factName = const "update"
instance Souffle.Fact Call              where factName = const "call"
instance Souffle.Fact CallArgument      where factName = const "callargument"
instance Souffle.Fact NodePattern       where factName = const "nodepattern"
instance Souffle.Fact NodeParameter     where factName = const "nodeparameter"
instance Souffle.Fact Case              where factName = const "case"
instance Souffle.Fact Alt               where factName = const "alt"
instance Souffle.Fact AltLiteral        where factName = const "altliteral"
instance Souffle.Fact AltDefault        where factName = const "altdefault"
instance Souffle.Fact ReturnValue       where factName = const "returnvalue"
instance Souffle.Fact FirstInst         where factName = const "firstinst"
instance Souffle.Fact NextInst          where factName = const "nextinst"
instance Souffle.Fact FunctionParameter where factName = const "functionparameter"
instance Souffle.Fact AltParameter      where factName = const "altparameter"


gtagToDtag :: Grin.Tag -> Tag
gtagToDtag (Grin.Tag tt name) = (renderTagType tt) <> Grin.nameText name
  where
    renderTagType :: Grin.TagType -> Text
    renderTagType Grin.C      = "C"
    renderTagType Grin.F      = "F"
    renderTagType (Grin.P m)  = "P-" <> Text.pack (show m) <> "-"

-- type Souffle a = ReaderT (Handle HPT) SouffleM a

convertExternals :: [Grin.External] -> SouffleM ()
convertExternals = undefined

emitAlg :: Handle HPT -> Grin.ExpF (Grin.Exp, SouffleM ()) -> SouffleM ()
emitAlg prog = \case
  Grin.ProgramF externals defs -> do
    convertExternals externals
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
    Souffle.addFact prog $ litAssignFact res l
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
            (\j a -> AltParameter (Grin.nameText cs_res) (gtagToDtag tag) j {-(Grin.nameText a)-})
            [0..]
            args

      -- TODO: Handle literals better
      -- Grin.Alt (Grin.LitPat (G.SInt64 i)) n _ ->
      --   [ AltLiteral
      --       { case_result = Variable cs_res
      --       , alt_value = Variable n
      --       , l = Literal (show i)
      --       }
      --   ]

      Grin.Alt Grin.DefaultPat n _ -> do
        Souffle.addFact prog $ AltDefault (Grin.nameText cs_res) (Grin.nameText n)

    rhs

litAssignFact :: Grin.Name -> Grin.Lit -> LitAssign
litAssignFact v sv = (\(ty, value) -> LitAssign (Grin.nameText v) ty value) $ case sv of
  Grin.LInt64  i -> (stToDatalogST Grin.T_Int64,  Text.pack $ show i)
  Grin.LWord64 w -> (stToDatalogST Grin.T_Word64, Text.pack $ show w)
  Grin.LFloat  f -> (stToDatalogST Grin.T_Float,  Text.pack $ show f)
  Grin.LBool   b -> (stToDatalogST Grin.T_Bool,   Text.pack $ show b)
  Grin.LChar   c -> (stToDatalogST Grin.T_Char,   Text.pack $ show c)
  Grin.LString s -> (stToDatalogST Grin.T_String, s) -- TODO

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
