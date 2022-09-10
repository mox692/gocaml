let main =
  let () = print_endline "\nEnter program ..." in
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      let _ = Printf.printf "=> %d\n" result in
      print_newline
      ()
    done
  with 
    Lexer.EOF -> exit 0
