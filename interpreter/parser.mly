%{
  open Types
%}

%token <float> FLOAT
<<<<<<< HEAD
%token TRUE FALSE 
%token DBLSEMI
%nonassoc FLOAT


=======
%token DBLSEMI
%nonassoc FLOAT

>>>>>>> 17b0c067ccbf27a7f4c5760788690d53093424da
%start main
%type <Types.exprS> main
%%

main:
  | headEx DBLSEMI               { $1 }
;

headEx:
  | expr                         { $1 }
;

expr:
  | FLOAT                        { NumS $1 }
<<<<<<< HEAD
  | TRUE 						 { BoolS true} 
  | FALSE 						 { BoolS false} 
=======
>>>>>>> 17b0c067ccbf27a7f4c5760788690d53093424da
;

