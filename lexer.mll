{
    exception EOF 
    open Parser
}

let ident = ['a'-'z' 'A'-'Z']*

rule token = parse
    [' ' '\t']          { token lexbuf }
  | ['\n' ]             { EOL }
  | ['0'-'9']+ as lxm   { INT(int_of_string lxm) }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { STAR }
  | '/'                 { SLASH }
  | '('                 { LBRC }
  | ')'                 { RBRC }
  | '<'                 { RB }
  | '>'                 { LB }
  | ">="                { LBEQ }
  | "<="                { RBEQ }
  | '{'                 { LCBC }
  | '}'                 { RCBC }
  | ';'                 { SMCLN }
  | ':'                 { CLN }
  | '.'                 { COMMA }
  | '='                 { EQ }
  | "ave"               { Ave }
  | "if"                { IF }
  (* | "else if"            { IF } *)
  | "else"              { ELSE }
  | "for"               { FOR }
  | "var"               { VAR }
  | "struct"            { STRUCT }
  | "type"              { TYPE }
  | "int"               { INT_T }
  | ident* as lexeme    { IDENT (lexeme) }
  | eof                 { EOF }
  
