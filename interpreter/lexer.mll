{
  open Parser
  exception Eof
  exception Unrecognized
}

let any = _
let digit = ['0'-'9']
let sign = ['+' '-']
let frac = '.' digit+
let exp = ['e' 'E'] sign? digit+
let white = [' ' '\t' '\n' '\r']+ | "//" ([^ '\n' '\r'])*
let newline = '\n' | '\r' | "\r\n"
let dblsemi = ";;"
let float = (digit+ '.'? | digit* frac) exp?
<<<<<<< HEAD
let true = "true" | "#t" 
let false = "false" | "#f" 

=======
>>>>>>> 17b0c067ccbf27a7f4c5760788690d53093424da

rule token = parse
  | white       { token lexbuf }
  | newline     { token lexbuf }
  | dblsemi     { DBLSEMI }
  | float as x  { FLOAT (float_of_string x) }
<<<<<<< HEAD
  | true 		{ TRUE } 
  | false 		{ FALSE }
=======
>>>>>>> 17b0c067ccbf27a7f4c5760788690d53093424da
  | eof         { raise Eof }
  | any         { raise Unrecognized }
