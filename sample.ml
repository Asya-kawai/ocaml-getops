(* In repl, #use "getopt.ml" *)
(**
  If you use ocaml files in others directory,
  copy below script into ./dune.
  (copy files# <path>/<to>/<your>/<directory>/*.{ml,mli})
*)

let my_prog verbose number () =
  Printf.printf "verbose_option = %s\n" (string_of_bool verbose);
  Printf.printf "number option = %s\n" (string_of_int number)

let () =
  let command_line_arguments = [|"-v"; "--number"; "100"|] in

  let verbose =
    let v_opt =
      Getopt.M.Bool_t (Getopt.BoolM.create ~short:"v" ~long:"verbose" ())
    in
    let v =
      Getopt.parse ~options:command_line_arguments ~option_type:v_opt
    in
    Getopt.M.get_bool_t ~t:v
  in
  let number =
    let n_opt =
      Getopt.M.Int_t (Getopt.IntM.create ~short:"n" ~long:"number" ())
    in
    let n =
      Getopt.parse ~options:command_line_arguments ~option_type:n_opt
    in
    Getopt.M.get_int_t ~t:n
  in
  my_prog verbose.value number.value ()
