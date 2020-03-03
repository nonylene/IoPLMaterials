%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ

%token <int> INTV
%token <Syntax.id> ID

%start <Syntax.program> sentences toplevel
(* %type <Syntax.program> toplevel *)
(* %type <Syntax.sentences> sentences *)
%%

toplevel:
  e=sentences { e }
  | e=sentences s=toplevel { e @ s }

sentences:
  e=Expr SEMISEMI { [Exp e] }
  | e=Declrs SEMISEMI { e }

(* split into declr list *)
Declrs :
  e=Declr { [e] }
  | e=Declr s=Declrs { e::s }

Declr :
  LET x=ID EQ e=Expr { Decl (x, e) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=ANDExpr { e }

LetExpr :
  LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

ANDExpr : 
    l=ANDExpr AND r=ORExpr { BinOp (And, l, r) }
  | e=ORExpr { e }

ORExpr : 
    l=ORExpr OR r=LTExpr { BinOp (Or, l, r) }
  | e=LTExpr { e }

LTExpr : 
    l=LTExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr : 
    l=MExpr MULT r=AExpr { BinOp (Mult, l, r) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
