%{
open Type
open Utils
let defaultHashtblSize = 1000
let symbolTbl: (string, Type.gocaml_value) Hashtbl.t ref = ref (Hashtbl.create defaultHashtblSize)
let typeTbl = ref (Type.new_type_tbl defaultHashtblSize)
%}

%left PLUS MINUS
%token PLUS MINUS
%token STAR SLASH
%token EOL
%token Ave
%token ELSE
%token EOF
%token IF
%token FOR
%token VAR
%token COMMA    // .
%token EQ       // =
%token RB       // > 
%token LB       // >
%token LBEQ     // >=
%token RBEQ     // <=
%token RBRC     // (
%token LBRC     // )
%token LCBC     // {
%token RCBC     // }
%token SMCLN    // ;
%token CLN      // :
%token STRUCT
%token TYPE
%token INT_T
%token <int> INT
%token <string> IDENT
%start main
%type <int> main
%%

main:
    stmt EOL { $1 }
;

stmts:
    stmt { $1 }
|   EOL stmt { $2 }
|   stmt EOL stmts { let _ = $1 in 0 }
|   EOL stmts { $2 }
;

// TODO: equalityを、primitive型によって調整
stmt:
    decl { $1 }
|   decl_type { $1 }
|   if_stmt { $1 }
|   for_stmt { $1 }
|   assign   { $1 }
|   decl_assign { $1 }
|   equality {
        match $1 with
        | Type.INT(v) -> Type.get_int_val v
        | _ -> print_endline "abort in stmt"; exit 1;
    }
;

assign:
    IDENT EQ equality {
        try
            let _ = Hashtbl.find !symbolTbl $1 in
            let _ = Hashtbl.add !symbolTbl $1 $3 in
            0
        with
        |   Not_found  -> Printf.printf "error, undeclared variable `%s`\n" $1; -1
    }
;

decl:
    // TODO: 一応、ここはbuiltin型のみを考えるように
    VAR IDENT IDENT { 
        let type_name = Type.find_built_in_type $3 in
        match type_name with
        | "int" -> Hashtbl.add !symbolTbl $2 (Type.built_in_default_value "int"); 0
        | _     -> Printf.printf "Type %s is not exist." type_name; 0
    }
;

decl_type:
    decl_struct { $1 }
|   decl_interface { $1 }
;

decl_struct:
    TYPE IDENT STRUCT block_type_field {
        (* TODO: struct の type tableの追加 *)
        let struct_name = $2 in
        try
            let _ = Hashtbl.find !typeTbl struct_name in
            let _ = Printf.printf "struct `%s` is already defined.\n" struct_name in
            0
        with
        | Not_found -> 
            let field_list: Type.field list = [] in
            let _ = Hashtbl.add !typeTbl struct_name field_list in
            let operations = $4 in
            let rec run list = 
                match list with
                | [] -> ();
                | h :: t -> h(struct_name); run t in
            let _ = run operations in
            0
    }
;

block_type_field:
    LCBC stmts_field RCBC { 
        let rec length list n = 
            match list with
            | [] -> ();
            | h :: t -> length t (n+1) in
        let _ = length $2 0 in
        $2
    }
;

stmts_field:
    stmt_field { [$1] }
|   EOL stmt_field { [$2] }
|   stmt_field EOL { [$1] }
|   stmt_field EOL stmts_field { $1 :: $3 }
|   EOL stmts_field { $2 }
;

stmt_field:
    IDENT INT_T {
        fun struct_name -> 
            let field_symbol = $1 in
            try
                let new_field: field = Type.Field(field_symbol, "int") in
                let current_field_list = Hashtbl.find !typeTbl struct_name in
                let _ = Hashtbl.add !typeTbl struct_name  (new_field :: current_field_list) in
                let current_field_list = Hashtbl.find !typeTbl struct_name in
                0
            with
            | Not_found -> 
                let _ = Printf.printf "struct %s is not defined." struct_name in
                exit(1)
            | _ ->
                let _ = Printf.printf "Error unknown reason" in
                exit(1)
    }
;

// TODO: impl
decl_interface:
    { 0 }
;

decl_assign:
    IDENT CLN EQ equality { 
        try
            let _ = Hashtbl.find !symbolTbl $1 in
            let _ = Printf.printf "error, variable `%s` is already declared\n" $1 in
            (-1)
        with
        |   Not_found  -> Hashtbl.add !symbolTbl $1 $4; 0
    }
|   IDENT CLN EQ struct_initialize { 
        (* type table *)
        try
            let _ = Hashtbl.find !symbolTbl $1 in
            let _ = Printf.printf "variable %s is already declared.\n" $1 in
            0
        with
        | Not_found -> 
            let _ = Hashtbl.add !symbolTbl $1 (Type.new_struct $1) in
            let rec f func_list = 
                match func_list with
                | [] -> ()
                | h :: t -> h($1); f t in
            let _ = f($4) in
            0
    }
;

// symbol
struct_initialize:
    IDENT struct_initialize_block { 
        (*
            struct_initialize_blockからは関数を返す関数のlistが返ってくる.
            それを1つずつ展開して、関数のlistを返すようにする
        *)
        let rec map_func fun_list =
            match fun_list with
            | [] -> []
            | h :: t -> h($1) :: (map_func t) in

        map_func $2
    }
;

struct_initialize_block:
    LCBC struct_initialize_stmts_field RCBC { $2 }
;

struct_initialize_stmts_field:
    struct_initialize_stmt_field { [$1] }
|   EOL struct_initialize_stmt_field { [$2] }
|   struct_initialize_stmt_field EOL { [$1] }
|   struct_initialize_stmt_field EOL struct_initialize_stmts_field { $1 :: $3 }
|   EOL struct_initialize_stmts_field { $2 }
;

struct_initialize_stmt_field:
    IDENT CLN atom {
        fun struct_name -> 
            fun val_name -> 
                let field = $1 in
                let v = $3 in
                try
                    (* 型が存在するか *)
                    let field_list = Hashtbl.find !typeTbl struct_name in
                    let Field(sym, typ) = Type.find_field_by_field_name field_list field in
                    (* TODO: typeCheck *)

                    (* val tableから現在のstructの情報を取得 *)
                    let cur_struct_value = Hashtbl.find !symbolTbl val_name in
                    let STRUCT(GocamlStruct(_, cur_struct_field_val)) = cur_struct_value in

                    (* 更新を加えた新しいstructを作成 *)
                    let new_field_val = Type.FieldVal(sym, (Type.buitl_in_to_ocaml v)) in
                    let new_struct = Type.new_struct_with_field_list struct_name (new_field_val :: cur_struct_field_val) in

                    (* symbol tableに追加 *)
                    let _ = Hashtbl.add !symbolTbl val_name new_struct in
                    0
                with
                | Not_found ->
                    let _ = Printf.printf "struct %s is not declared\n" struct_name in
                    (-1)
    }
;

for_stmt:
    {0}
;

if_stmt:
|   if_single { $1 }
|   if_else { $1 }
;

// if 3 > 2 { ... }
if_single:
    IF equality block {
        match $2 with
        | Type.INT(v) ->
            if Type.get_int_val v == 1
            then $3
            else 0
        | _ -> print_string "abort in if_single"; exit 1;
    }
;

if_else:
    IF equality block ELSE block {
        match $2 with
        | Type.INT(v) ->
            if Type.get_int_val v == 1
            then $3
            else $5
        | _ -> print_string "abort in if_single"; exit 1;
    }
;

block:
    LCBC stmts RCBC { $2 }
|   EOL LCBC stmts RCBC { $3 }
;
    
// ここで全てINTにする
// TODO: stringで""だったら0にする、みたいな仕様はあり？
equality:
|   expr { 
        match $1 with
        | Type.INT(v) -> Type.INT(v)
        | Type.STRING(_) -> INT(Type.gen_int 0)
    }
|   expr EQ EQ expr { Type.eval_equality_int($1,$4,"==") }
|   expr RB expr { Type.eval_equality_int($1,$3,"<") }
|   expr LB expr { Type.eval_equality_int($1,$3,">") }
|   expr LBEQ expr { Type.eval_equality_int($1,$3,">=") }
|   expr RBEQ expr { Type.eval_equality_int($1,$3,"<=") }  
;

expr:
    add_sub { $1 }
;

add_sub:
    mul_div { $1 }
|   add_sub PLUS add_sub { Type.binary_calc_operation($1,$3,"+") }
|   add_sub MINUS add_sub { Type.binary_calc_operation($1,$3,"-") }

mul_div:
    atom { $1 }
|   mul_div STAR mul_div { Type.binary_calc_operation($1,$3,"*") }
|   mul_div SLASH mul_div { Type.binary_calc_operation($1,$3,"/") }
;

atom:
    INT { Type.INT(Type.gen_int $1) }
|   MINUS INT { Type.INT(Type.gen_int ((-1)*$2)) }
|   IDENT { Hashtbl.find !symbolTbl $1 }
|   IDENT COMMA IDENT {
        try
            let v = Hashtbl.find !symbolTbl $1 in
            (* val tableからfieldの値を引っ張ってくる *)
            match v with 
            | STRUCT(GocamlStruct(struct_name, field_list)) ->
                let _ = print_field_val_int field_list in
                let vv = find_field_val_by_field_name $3 field_list in
                match vv with
                | FieldVal(fv) -> 
                    match fv with
                    | ("", _) -> Printf.printf "variable `%s` doesn't have field `%s`.\n" $1 $3; Type.INT(Type.gen_int 0)
                    | (_, OCamlInt(x)) -> Type.INT(Type.gen_int x)
                    | (_, OCamlString(x)) -> Type.STRING(Type.gen_string x)
                | _ -> 
                    let _ = Printf.printf "variable `%s` doesn't have field `%s`.\n" $1 $3 in
                    Type.INT(Type.gen_int 0)
            | _ -> 
                let _ = Printf.printf "variable `%s` doesn't have field `%s`.\n" $1 $3 in
                Type.INT(Type.gen_int 0)
        with
        | Not_found -> 
            let _ = Printf.printf "variable %s is not found.\n" $1 in
            Type.INT(Type.gen_int 0)
    }
;
