(* Module [Getopt]: parsing of command line arguments *)
(* Alain Frisch *)

let noshort = '\000'
let nolong  = ""

type opt = char * string * ((unit -> unit) option) * ((string -> unit) option)

exception Error of string

let index_option s c =
  try Some (String.index s c)
  with Not_found -> None

let extract_arg_handle opt = function
  | (_,_,_,Some handle) -> handle
  | _ -> raise (Error (Printf.sprintf 
			 "Option %s does not accept argument" opt))

let extract_handle opt = function
  | (_,_,Some handle,_) -> handle
  | _ -> raise (Error (Printf.sprintf 
			 "Option %s must have an argument" opt))



(* opts がわかりにくいのでモジュールにするべき *)
(*

  short option : string
  long option  : string

  arg  option argument : list 
  引数が与えられるパターン
  list [1;2;3;]
  引数が与えられないパターン
  []

*)

module BoolOption = struct
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

module BoolOption2 = struct
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
end

(* options: list *)
module Option = struct
  type t =
    | Bool of BoolOption.t
    | Bool2 of BoolOption2.t
    | None
  let show ?(options=[]) ?(t=None) () = t
end
(** How to use;;;;
    Option.show ~t:(Option.Bool (BoolOption.create ())) ();;
*)

let is_short_option ~option_str =
  let str_len = String.length option_str in
  if (str_len != 2) then false (* Invaild short option *)
  else (
    if option_str.[0] = '-' && option_str.[1] != '-' then true
    else false
  )

let is_long_option ~option_str = 
  let str_len = String.length option_str in
  (* This is short option or invalid option *)
  if (str_len <= 2) then false
  else (
    if option_str.[0] = '-' && option_str.[1] = '-' &&
       option_str.[2] != '-' then true
    else false
  )

let set_option ~options ~option_type =
  (* options array converts to list *)
  let opts = Array.to_list(options) in
  let rec _get_options opts =
    match opts with
    | [] -> option_type
    | opt1 :: [] -> 
      if opt1 = option_type.short then
        option_type.set_value true
      else
        _get_options []
    | opt1 :: opt2 :: opt_rest ->
      let str_len = String.length opt in
      if (opt.[0] = '-') && opt = ("-" ^ option_type.short) then
        option_type.set_value option_type opt_value
      else        
        _get_options opt_rest
  in _get_options opts 

let parse opts others args first last =
  let find_long opt =
    try List.find (fun (_,l,_,_) -> opt = l) opts 
    with Not_found ->
      raise (Error (Printf.sprintf "Unknown option --%s" opt))
  in
  let find_short opt =
    try List.find (fun (l,_,_,_) -> opt = l) opts 
    with Not_found ->
      raise (Error (Printf.sprintf "Unknown option -%c" opt))
  in

  (* Anonymous arguments after -- *)
(* -- の後ろの引数をスキップする?? *)
(* others に args.(no) を与えているようにみえる *)
(* そのあとで、succ にて +1 したものを skip にわたするーぷする *)
  let rec skip no =
    if (no <= last) then (others args.(no); skip (succ no)) in

  let rec aux no =
    if (no <= last) then
      let s = args.(no) in
      let l = String.length s in
      if (l=0) then (others s; aux (succ no))
      else if  (s.[0] = '-') then
	      if (l >= 2) && (s.[1] = '-') then
	       if (l = 2) then skip (succ no) (* -- *)
	      else match index_option s '=' with
	       | Some i -> (* long option with argument *)
		      let opt = String.sub s 2 (i-2) in
		    let arg = String.sub s (i+1) (l-i-1) in
		    let handle = extract_arg_handle ("--"^opt) (find_long opt) in
		    handle arg;
		  aux (succ no)
	      | None ->  (* long option with no argument *)
		let opt = String.sub s 2 (l-2) in
		let handle = extract_handle s (find_long opt) in
		handle ();
		aux (succ no)
	else if (l = 1) then (others s; aux (succ no))  (* - *)
	else (* short option *)
	  let opt = s.[1] in
	  match find_short opt with
	    | (_,_,Some handle,None) ->
		(* no argument allowed; next chars are options *)
		handle ();
		for i = 2 to (l - 1) do
		  match find_short s.[i] with
		    | (_,_,Some handle,None) -> handle ()
		    | _ -> raise (Error (Printf.sprintf 
					   "Only non-argument short-options can be concatenated (error with option %c in %s)"  s.[i] s))
		done;
		aux (succ no)
	    | (_,_,_,Some handle) as o ->
		(* argument allowed or mandatory *)
		if (l>2) then (* immediate argument *)
		  (handle (String.sub s 2 (l-2)); aux (succ no))
		else if (no+1 <= last) && (args.(no+1).[0] <> '-') then
		  (* non-immediate argument *)
		  (handle args.(no+1); aux (no+2))
		else 
		  (* no argument *)
		  let handle = extract_handle s o in
		  (handle (); aux (succ no))
	    | _ -> failwith "Getopt.parse"
      else
	(others s; aux (succ no))
  in
  aux first



let parse_cmdline opts others =
  parse opts others Sys.argv 1 (Array.length Sys.argv - 1)

(* options は コマンドライン引数 をとる。 コマンドライン引数はリストで受け取りたいかも *)
let parse_options options = 
  (* こんな感じで使えると良いな *)
  let opt_of_verbose = BoolOption.create ~short:"v" ~long:"verbose" () in
  (* set_opt: コマンドライン引数 -> セットしたいオプション型 -> 値が設定されたオプション型 *)
  let verbose = set_opt options opt_of_verbose in
  ...
  


(* useful actions and handlers *)

let set var value = Some (fun () -> var := value)

let append lst = Some (fun x  -> lst := !lst@[x])

let incr var = Some (fun () -> Pervasives.incr var)

let atmost_once var exc = Some (fun x -> if !var="" then var := x else raise exc)

