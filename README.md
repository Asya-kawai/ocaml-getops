## What's is this?
Getopt is parser of command_line_arguments.  
It's consists single module file 'getopt.ml',
Getopt module includes below.

* Sub module 'M' to manage command_line_argument option(short and long option) and its value.
* Methods of sub module 'M' to set and get a option value.
* Sub sub module 'XXX M' to consist Sub module 'M'.
* Function of parse to parse command_line_arguments and set sub module 'M' type as specific record type.

## Install from Github

```
git clone https://github.com/Asya-kawai/ocaml-getopt.git
```

## Dependencies

Nothing!  
This module is realy simple!

## Usage of Getopt module

Just copy getopt.ml in your project directory.

```
cp ocaml-getopt/getopt.ml <path>/<of>/<your>/<project>/.
```

## Example to use Getopt module

```
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
```
