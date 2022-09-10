(* util functions *)

let rec list_len l n = 
  match l with
  | [] -> n
  | h :: t -> list_len t (n+1)

let rec list_push l e =
  match l with
  | [] -> print_string "unexpected error, exit."; exit 1
  | h :: t -> 
    if list_len t 0 == 0
    then
      h :: e :: []
    else
      h :: (list_push t e)

let rec print_int_list l str =
  match l with
  | [] -> str
  | h :: t -> 
    let s = Printf.sprintf "%d, " h in
    print_int_list t (str ^ s)
  
(* test *)
let test_list_push =
  let test_case = [
    ([1;2;3], 4, [1;2;3;4]);
    ([1;], 4, [1;4])
  ] in

  let rec run_test cases =
    match cases with
    | [] -> ()
    | h :: t ->
      let (l, e, expected) = h in
      let got = list_push l e in
      if got <> expected
      then
        let _ = Printf.printf "unmatch, list got content: %s\nexpect content: %s" (print_int_list got "") (print_int_list expected "") in
        ()
      else
        run_test t in

  let _ = run_test test_case in
  ()
