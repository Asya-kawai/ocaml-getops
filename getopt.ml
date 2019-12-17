(* Module [Getopt]: parsing of command line arguments *)
(* Toshiki Kawai *)

exception Error of string

(**
   Coding rule:

   M is module.
   SomethingM means Something is Module.

   Only M is main module of this file.
   M is use from others file such as 'Option.M.method_name arguments'.

   _t is type.
   Something_t means the type of Something module.
   
*)

(**
   Basic module definition.
*)

module BoolM =struct
  type t = {
    short: string;
    long: string;
    value: bool;
  }
  let create ?(short="") ?(long="") ?(value=false) () =
    {
      short;
      long;
      value;
    }
  let set_short t short_option = { t with short = short_option }
  let set_long t long_option = { t with long = long_option }
  let set_value t value = { t with value = value }
end

module StringM =struct
  type t = {
    short: string;
    long: string;
    value: string;
  }
  let create ?(short="") ?(long="") ?(value="") () =
    {
      short;
      long;
      value;
    }
  let set_short t short_option = { t with short = short_option }
  let set_long t long_option = { t with long = long_option }
  let set_value t value = { t with value = value }
end

module IntM =struct
  type t = {
    short: string;
    long: string;
    value: int;
  }
  let create ?(short="") ?(long="") ?(value=0) () =
    {
      short;
      long;
      value;
    }
  let set_short t short_option = { t with short = short_option }
  let set_long t long_option = { t with long = long_option }
  let set_value t value = { t with value = value }
end

module FloatM =struct
  type t = {
    short: string;
    long: string;
    value: float;
  }
  let create ?(short="") ?(long="") ?(value=0.) () =
    {
      short;
      long;
      value;
    }
  let set_short t short_option = { t with short = short_option }
  let set_long t long_option = { t with long = long_option }
  let set_value t value = { t with value = value }
end

(* Interface and methods *)
module M = struct
  type t =
    | Bool_t of BoolM.t
    | String_t of StringM.t
    | Int_t of IntM.t
    | Float_t of FloatM.t
    | None_t
  let show ?(t=None_t) () = t
  let set_short ?(t=None_t) ~value =
    match t with
    | Bool_t x -> Bool_t { x with short = value }
    | String_t x -> String_t { x with short = value }
    | Int_t x -> Int_t { x with short = value }
    | Float_t x -> Float_t { x with short = value }
    | None_t -> None_t
  let set_long ?(t=None_t) ~value =
    match t with
    | Bool_t x -> Bool_t { x with long = value }
    | String_t x -> String_t { x with long = value }
    | Int_t x -> Int_t { x with long = value }
    | Float_t x -> Float_t { x with long = value }
    | None_t -> None_t
  let set_value ?(t=None_t) ~value =
    match t with
    | Bool_t x -> Bool_t { x with value = (bool_of_string value) }
    | String_t x -> String_t { x with value = value }
    | Int_t x -> Int_t { x with value = (int_of_string value) }
    | Float_t x -> Float_t { x with value = (float_of_string value) }
    | None_t -> None_t
  let get_bool_t ~t =
    match t with
    | Bool_t x -> x
    | _ -> raise (Error "argument ~t is must be Bool_t.")
  let get_string_t ~t =
    match t with
    | String_t x -> x
    | _ -> raise (Error "argument ~t is must be String_t.")
  let get_int_t ~t =
    match t with
    | Int_t x -> x
    | _ -> raise (Error "argument ~t is must be Int_t.")
  let get_float_t ~t =
    match t with
    | Float_t x -> x
    | _ -> raise (Error "argument ~t is must be Float_t.")
end

(** How to use;;;;
    M.show ~t:(M.Bool_t (BoolM.create ())) ();;
*)

let is_short ~option_str =
  let str_len = String.length option_str in
  if (str_len != 2) then false (* Invaild short option *)
  else (
    if option_str.[0] = '-' && option_str.[1] != '-' then true
    else false
  )

let is_long ~option_str = 
  let str_len = String.length option_str in
  (* This is short option or invalid option *)
  if (str_len <= 2) then false
  else (
    if option_str.[0] = '-' && option_str.[1] = '-' &&
       option_str.[2] != '-' then true
    else false
  )

(**
   parse gets options(Sys.argv as array) and option_type(M.t),
   returns record of M.t.

   Arguments:
    ~options: command line argument (Sys.argv).
    ~option_type: M.T (M.Bool_t, M.String_t...)

   Usage:
     let options = Sys.argv in
     let option_of_verbose = M.Bool_t (BoolM.create ()) in
     let verbose = parse options option_of_verbose in
     ...
     your_program ~verbose:verbose ...
     ...
*)
let parse ~options ~option_type =
  (* options array converts to list *)
  let opts = Array.to_list(options) in
  let rec _get_options opts =
    match opts with
    | [] -> option_type
    | opt1 :: [] -> (
        match option_type with
        | M.Bool_t x ->
          if x.short = opt1 || x.long = opt1 then
            M.set_value ~t:option_type ~value:"true"
          else
            option_type
        | _ -> option_type (* No set anything, option invalid perhaps. *)
      )
    | opt1 :: opt2 :: opt_rest ->
      let is_option opt = is_short ~option_str:opt || is_long ~option_str:opt in
      let is_correct option_type v = match option_type with
        | M.Bool_t x -> "-" ^ x.short = v || "--" ^ x.long = v
        | M.String_t x -> "-" ^ x.short = v || "--" ^ x.long = v
        | M.Int_t x -> "-" ^ x.short = v || "--" ^ x.long = v
        | M.Float_t x -> "-" ^ x.short = v || "--" ^ x.long = v
        | M.None_t -> false
      in 
      if  (is_option opt1) then
        (* pattern: --option1 --option2 ... *)
        if (is_option opt2) then
          match option_type with
          | M.Bool_t _ ->
            if is_correct option_type opt1 then
              M.set_value ~t:option_type ~value:"true"
            else
              _get_options opt_rest
          | M.None_t ->
            raise (Error "Option 'None_t' does't be acceptable option_type.")
          | _ -> _get_options (opt2 :: opt_rest)
        else
          (* pattern: --option1 value1 ... *)
          match option_type with
        | M.Int_t _ ->
          if is_correct option_type opt1 then
            M.set_value ~t:option_type ~value:opt2
          else
            _get_options opt_rest
        | M.String_t _ ->
          if is_correct option_type opt1 then
            M.set_value ~t:option_type ~value:opt2
          else
            _get_options opt_rest
        | M.Float_t _ ->
          if is_correct option_type opt1 then
            M.set_value ~t:option_type ~value:opt2
          else
            _get_options opt_rest
        | M.Bool_t _ ->
          raise (Error (
              Printf.sprintf "Option '%s' does't have an argument, got '%s'."
                opt1 opt2
            ))
        | M.None_t ->
          raise (Error "Option 'None_t' does't be acceptable option_type.")
      else
        raise (Error (
              Printf.sprintf "Option '%s' is invalid optional format, should '-' or '--' in string of head."
                opt1
            ))
  in _get_options opts

(**
   Testing:

   Note: short_option is first char is '-'.
         long_option is first and second char is '-'.
         SomethingM's ~short and ~long is string except of '-'.
         Example is below.

   let options = [|"-v"; "-n"; "1"|] ;;
   let v = M.Bool_t (BoolM.create ~short:"v" ~long:"verbose" ()) ;;
   parse ~options:options ~option_type:v
*)
