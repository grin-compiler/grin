Inductive Lit : Set :=
| LInt64 : nat  -> Lit
| LWord64: nat  -> Lit
| LBool  : bool -> Lit
.

Inductive TagType : Set :=
| C : TagType
| F : TagType
| P : nat -> TagType
.
 
Inductive Name : Set :=
| NName : nat -> Name
.

Inductive Tag : Set :=
| TTag : TagType -> Name -> Tag
.

Inductive SimpleVal : Set :=
| SVLit : Lit  -> SimpleVal
| SVVar : Name -> SimpleVal
.

Inductive Val : Set :=
| ConstTagNode : Tag -> list SimpleVal -> Val
| Unit         : Val
| Simple       : SimpleVal -> Val
.

Inductive CPat : Set := 
| NodePat : Tag -> (list Name) -> CPat
| LitPat  : Lit -> CPat
.

Inductive Exp : Set :=
| Bind : SExp -> Val -> Exp -> Exp
| Case : Val  -> list (CPat * Exp) -> Exp
| Sexp : SExp -> Exp
with SExp : Set :=
| App    : Name -> list SimpleVal -> SExp
| Pure   : Val  -> SExp 
| Store  : Val  -> SExp
| Fetch  : Name -> SExp
| Update : Name -> Val -> SExp
| Block  : Exp  -> SExp
.

Inductive Def : Set := 
| Fun : Name -> (list Name) -> Exp -> Def
.

Definition Program : Set := list Def.
