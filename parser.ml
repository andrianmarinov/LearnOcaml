type 'a parser = { run: char list -> ('a * char list) option }

let run p s = p.run (String.to_list s)

let mk_parser run = { run }

let fail = mk_parser (fun _ -> None)

let parse_char c  = 
  mk_parser ( fun cs ->
  match cs with
  | [] -> None
  | c' :: tl -> if c' = c then Some (c', tl) else None
)

(* Also returns a parser *)
let either p1 p2 = 
  mk_parser ( fun cs ->
    match p1.run cs with
    | Some x -> Some x 
    | None -> p2.run cs
  )
(* Returns a parser *)
let map f p =
  mk_parser @@ fun cs ->
    match p.run cs with
    | Some (x, rest) -> Some (f x, rest) 
    | None -> None


let p = parse_char

let result = run (p 'a') "asd"

let p_a = either (p 'a') (p 'A')

let q = map Char.is_lowercase p_a

(* Any char *)
let any = 
  mk_parser @@ fun cs ->
    match cs with
    | [] -> None
    | x::xs -> Some (x, xs)

(* Takes parser of type a and returns a parser of type b *)

(* val many : 'a parser -> ('a list) parser *)

let many p =
  mk_parser @@ fun cs ->
    let rec aux acc cs =
      match p.run cs with
      | Some (x, xs) -> 
        let new_acc = x :: acc in
        aux new_acc xs
      | None ->
        Some (List.rev acc, cs)
    in
    aux [] cs

(* val string_p : string -> string parser *)
(* This sux. Make it parse Hello World *)
let string_p s =
  mk_parser @@ fun cs ->
    if (String.to_list s) = cs then Some(s, [])
    else None

(* Always succeed and does not consume *)
(* val yield : 'a -> 'a parser  *)
let yield c =
  mk_parser @@ fun cs ->
  Some(c, cs)
  
(* val filter : ('a -> bool) -> 'a parser -> 'a parser *)
let filter f p =
  mk_parser @@ fun cs ->
    match p.run cs with
    | Some (x, xs) -> 
      if f x then Some(x,xs)
      else None
    | None -> None

let (>>) f g = fun x -> g (f x) 

(* val maybe : 'a parser -> ('a option) parser *)
let maybe p = mk_parser @@ fun cs ->
  p.run cs
  |> Option.map (fun (x,cs) -> (Some x, cs))
  
(*
  mk_parser @@ fun cs ->
    match p.run cs with
    | Some(x, xs) -> Some( Some(x), xs)
    | None -> None
    *)

(* val many_one : 'a parser -> ('a list) parser * MAKE IT WORK + REWRITE IN BIND *)
let many_one p =
  mk_parser @@ fun cs ->
    let rec aux acc cs = 
      match p.run cs with
      | Some(x, xs) -> 
        let new_acc = x :: acc in
        aux new_acc xs
      | None -> None
    in
    aux [] cs
(* ?????????????????????????????????????????? *)
let many_one1 p =
  p >>= fun x ->
  many p >>= fun xs ->
  yield (x::xs)
  

(* val sequence : ('a parser) list -> ('a list) parser * REWRITE WITH BIND *)
let sequence p_list =
  mk_parser @@ fun cs ->
    let rec aux acc p_list cs =
      match p_list with
      | p :: ps -> (
        match p.run cs with
        | Some(x, xs) -> 
          let new_acc = x :: acc in
          aux new_acc ps xs
        | None -> None
      )
      | [] -> Some( List.rev acc, cs)
    in
    aux [] p_list cs

let p_list = [p_a; p_a]
let p_seq = sequence p_list

let result = run p_seq "absd";;


(* val bind : ('a -> 'b parser) -> 'a parser -> 'b parser *)
let bind f p =
  mk_parser @@ fun cs ->
  match p.run cs with
  | Some(x, xs) -> 
    let p2 = f x in 
    p2.run xs
  | None -> None

let (>>=) p f = bind f p

let my_parser =
  string_p "hello" >>= fun _ ->
  string_p "world" >>= fun _ ->
  int_p            >>= fun n ->
  ...


(* Build Polish Notation Calc *)
