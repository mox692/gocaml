open Utils

type field = Field of string * string
type field_list = field list

let rec print_field field_list =
  match field_list with
  | [] -> ()
  | Field(sym, typ) :: t -> 
    let _ = Printf.printf "symbol: %s, typ: %s\n" sym typ in
    print_field t

let rec find_field_by_field_name field_list field_name =
  match field_list with
  | [] -> Field("", "")
  | h :: t ->
    let Field(sym, typ) = h in
    if sym = field_name 
    then h
    else find_field_by_field_name t field_name

(* type table *)
let built_in_type = ["int"]

let rec _find_built_in_type type_name built_in_type_list = 
  match built_in_type_list with
  | [] -> ""
  | h :: t -> if h == h then h else _find_built_in_type type_name t

let find_built_in_type type_name =
  _find_built_in_type type_name built_in_type

let rec set_built_in_type_to_tbl type_tbl built_in_type_list = 
  match built_in_type_list with
  | [] -> type_tbl
  | h :: t -> Hashtbl.add type_tbl h []; set_built_in_type_to_tbl type_tbl t

let new_type_tbl size =
  set_built_in_type_to_tbl (Hashtbl.create size) built_in_type

(* TODO: structをnestできるように *)
type ocaml_val =
| OCamlInt of int
| OCamlString of string

type field_val = FieldVal of (string * ocaml_val) 

type gocaml_int = GocamlInt of (string * (string * int) list)
type gocaml_string = GocamlString of (string * (string * string) list)
type gocaml_struct = GocamlStruct of (string * field_val list)
type gocaml_value = 
  | INT of gocaml_int 
  | STRING of gocaml_string 
  | STRUCT of gocaml_struct

let rec print_field_val_int field_list =
  match field_list with
  | [] -> ()
  | FieldVal(sym, v) :: t -> 
    print_field_val_int t

let new_struct struct_name =
  STRUCT(GocamlStruct(struct_name, []))

let new_struct_with_field_list struct_name field_val_list =
  STRUCT(GocamlStruct(struct_name, field_val_list))

let rec find_field_val_by_field_name field_name field_val_list =
  match field_val_list with
  | [] -> FieldVal("", OCamlInt(0))
  | h :: t -> 
    let FieldVal(_field_name, _) = h in
    if field_name = _field_name
    then
      h
    else
      find_field_val_by_field_name field_name t

let generate_primitive_type type_name =
  fun value ->
    (type_name, [(type_name, value)])

let gen_int value: gocaml_int =
  GocamlInt((generate_primitive_type "int") value)

let gen_string value: gocaml_string = 
  GocamlString((generate_primitive_type "string") value)

let get_int_val (v: gocaml_int): int = 
  let GocamlInt(_, vlist) = v in
  match vlist with
  | h :: _ -> 
    let (_, x) = h in x
  | [] -> print_endline "Panic"; 0

let get_string_val (v: gocaml_string): string = 
  let GocamlString(_, vlist) = v in
  match vlist with
  | h :: _ -> 
    let (_, x) = h in x
  | [] -> print_endline "Panic"; ""

let binary_calc_operation (x, y, ope):gocaml_value =
  match x with
  | INT(a) -> 
    match y with
    | INT(b) -> 
      let n = match ope with
        | "+" -> get_int_val(a) + get_int_val(b)
        | "-" -> get_int_val(a) - get_int_val(b)
        | "*" -> get_int_val(a) * get_int_val(b)
        | "/" -> get_int_val(a) / get_int_val(b)
        | _   -> Printf.printf "operator `%s` is not allowed, error.\n" ope; exit 1 in
      INT(gen_int n)
    | _ -> print_endline "abort"; exit 1
  | _ -> print_endline "abort"; exit 1

let eval_equality_int (x, y, ope):gocaml_value =
  match x with
  | INT(_x) ->
      match y with
          | INT(_y) ->
              let ok = match ope with
                | "==" -> (get_int_val _x) == (get_int_val _y)
                | "<"  -> (get_int_val _x) < (get_int_val _y)
                | ">"  -> (get_int_val _x) > (get_int_val _y)
                | "<=" -> (get_int_val _x) <= (get_int_val _y)
                | ">=" -> (get_int_val _x) >= (get_int_val _y)
                | _  -> print_string "abort in equality_int"; exit 1 in
              if ok
              then INT(gen_int 1)
              else INT(gen_int 0)
          | STRING(_) ->
              print_endline "abort"; exit 0
  | STRING(_) ->
      print_endline "abort"; exit 0

let built_in_default_value (type_name:string): gocaml_value = 
  match type_name with
  | "int" -> INT(gen_int 0)
  | "string" -> STRING(gen_string "")
  | _ -> Printf.printf "Type %s is not defined.\n" type_name; exit 0

let print_variable_information (sym: gocaml_value) = 
  match sym with 
  | INT(v)    -> Printf.printf "int   : %d\n" (get_int_val v);
  | STRING(v) -> Printf.printf "string: %s\n" (get_string_val v);
  | STRUCT(v) -> 
    let GocamlStruct(type_name, field_val_list) = v in
    let _ = Printf.printf "Struct: %s\n" type_name in
    let rec print_struct_field_val field_val_list = 
      match field_val_list with
      | [] -> ()
      | h :: t -> 
        let FieldVal(field_name, ocaml_val) = h in
        let _ = match ocaml_val with
        | OCamlInt(t)    -> Printf.printf "%s: %d [int]\n" field_name t
        | OCamlString(t) -> Printf.printf "%s: %s [string]\n" field_name t in
        print_struct_field_val t in

    print_struct_field_val field_val_list

(* TODO: structが再帰で定義できるようになったら、修正する *)
let buitl_in_to_ocaml (v: gocaml_value): ocaml_val =
  match v with
  | INT(v)    -> OCamlInt(get_int_val v)
  | STRING(v) -> OCamlString(get_string_val v)
  | STRUCT(_) -> print_endline "Cannot convert struct to ocaml value."; exit 1


(* test *)
let rec check_built_in_type type_tbl expected_typ_list expected_val_list =
  match (expected_typ_list, expected_val_list) with
  | ([], []) -> ()
  | ([], _::_) -> ()
  | (_::_, []) -> ()
  | (h1 :: t1, h2 :: t2) -> 
    try
      let got = Hashtbl.find type_tbl h1 in
      if got == h2 
        then check_built_in_type type_tbl t1 t2
      else 
        (* fieldのlistをちゃんと導入した時にprintについては考える *)
        let _ = Printf.printf "errr\n" in
        exit (-1)
    with
    | Not_found -> Printf.printf "error, `%s` is not found\n" h1; exit (-1)

let test_set_built_in_type_to_tbl = 
  let tbl = set_built_in_type_to_tbl (new_type_tbl 1000) built_in_type in
  let _ = check_built_in_type tbl built_in_type [] in
  ()
