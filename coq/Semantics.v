Require Import List.
Require Import Syntax.
Require Import Domain.

Definition tagTypeDenote (t :TagType) : SemTagType :=
  match t with
  | C   => SC
  | F   => SF
  | P n => SP n
  end.

Definition tagDenote (t:Tag) : SemTag :=
  match t with
  | TTag typ (NName n) => STag (tagTypeDenote typ) (SName n)  
  end.

Definition nameDenote (n:Name) : SemName :=
  match n with 
  | NName n     => SName n
  end. 

Definition simpleDenote (r:Env) (v:SimpleVal) : Value :=
  match v with
  | SVLit (LInt64  n) => VInt  n
  | SVLit (LWord64 n) => VInt  n
  | SVLit (LBool   b) => VBool b
  | SVVar n =>
    match r with
    | EEnv f => f (nameDenote n)
    end
  end.

Definition valueDenote (r:Env) (v:Val) : Value :=
  match v with
  | Unit      => VUnit  
  | Simple s  => simpleDenote r s
  | ConstTagNode t vs => VNode (tagDenote t) (map (simpleDenote r) vs)
  end.  


(* TODO *)
Definition extEnvPat (r:Env) (p:Val) (v:Value) : Env :=
  match p with
  | ConstTagNode _ _    => r
  | Unit                => r
  | Simple (SVLit _)    => r
  | Simple (SVVar n)    => extendEnv r (nameDenote n) v 
  end.
 
Fixpoint extEnvPats (r:Env) (ls:list (SemName * Value)) : Env :=
  match ls with
  | nil         => r
  | (n,v) :: ls => extEnvPats (extendEnv r n v)ls
  end.

(*
Definition extEnvPats (r:Env) (ls:list (SemName * Value)) : Env :=
  fold_left (fun r' (n,v) => extendEnv r' n v) ls r. 
*)

Fixpoint selectAlt (v:Value) (ls : list (CPat * Exp)) 
  : option ((list (SemName*Value)) * Exp) := 
    match ls with
    | nil         => None
    | (p,e) :: ls =>
      match p with
      | NodePat t ns  => 
        match v with
        | VInt _      => selectAlt v ls
        | VWord _     => selectAlt v ls 
        | VBool _     => selectAlt v ls
        | VLoc _      => selectAlt v ls
        | VUnit       => selectAlt v ls
        | VBot        => selectAlt v ls
        | VNode st vs => if SemTagEq (tagDenote t) st   
          then selectAlt v ls    (* TODO *)
          else selectAlt v ls
        end 
      | LitPat lit    => selectAlt v ls  (* TODO *)
      end
    end.
 
 
Fixpoint expDenote (r:Env) (s:Heap) (e:Exp) {struct e}: Value * Heap :=
  match e with
  | Bind  le p re => 
      let (v,s') := sexpDenote r s le in (* TODO: use 'match' for simpler proofs *)
        let r' := extEnvPat r p v in
          expDenote r' s' re
  | Case v alts => 
    match selectAlt (valueDenote r v) alts with
    | None          => (VBot, s)
    | Some (nil, k) => expDenote r s k
    | Some (ls , k) => (VUnit, s) (*expDenote (extEnvPats r ls) s k  *)
    end
  | Sexp e'     => sexpDenote r s e'
  end
with sexpDenote (r:Env) (s:Heap) (e:SExp) : Value * Heap :=
  match e with
  | App _ _     => (VUnit, s)
  | Pure _      => (VUnit, s)
  | Store _     => (VUnit, s)
  | Fetch _     => (VUnit, s)
  | Update _ _  => (VUnit, s)
  | Block e'    => expDenote r s e'
  end.

 



