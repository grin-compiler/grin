Require Import Arith.


Inductive Loc : Set :=
| LLoc : nat -> Loc
.

Inductive SemTagType : Set :=
| SC : SemTagType
| SF : SemTagType
| SP : nat -> SemTagType
.

Definition SemTagTypeEq (t1 t2:SemTagType) : bool :=
  match t1, t2 with
  | SC, SC     => true
  | SF, SF     => true
  | SP n, SP m => beq_nat n m
  | _, _       => false
  end.

 
Inductive SemName : Set :=
| SName : nat -> SemName
.
 
Definition SemNameEq (n m:SemName) : bool :=
  match n with
  | SName n     =>
    match m with
    | SName m   => beq_nat n m
    end
  end.

Inductive SemTag : Set :=
| STag : SemTagType -> SemName -> SemTag
.

Definition SemTagEq (t1 t2:SemTag) : bool :=
  match t1, t2 with
  | STag t1 s1, STag t2 s2  => SemTagTypeEq t1 t2 && SemNameEq s1 s2
  end. 

Inductive Value : Set :=
| VInt  : nat    -> Value
| VWord : nat    -> Value 
| VBool : bool   -> Value
| VLoc  : Loc    -> Value
| VUnit : Value
| VBot  : Value
| VNode : SemTag -> list Value -> Value
.

Inductive Env : Set :=
| EEnv : (SemName -> Value) -> Env
.

Definition emptyEnv : Env := EEnv (fun n => VBot).

Inductive Heap : Set :=
| HHeap : nat -> (Loc -> Value) -> Heap
.

Definition emptyHeap : Heap := HHeap 0 (fun l => VBot).

Definition extendEnv (r:Env) (n:SemName) (v:Value) : Env :=
  EEnv (fun x => 
    if (SemNameEq x n) then v else 
      match r with  
      | EEnv f  => f x
      end)
.





