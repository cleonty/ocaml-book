let inc x = x + 1
let dec x = x - 1

let ( >> ) f g x = x |> f |> g

let inc_log x = (x + 1, Printf.sprintf "Called inc on %i; " x)
let dec_log x = (x - 1, Printf.sprintf "Called dec on %i; " x)

let dec_log_upgraded (x, s) =
  (x - 1, Printf.sprintf "%s; Called dec on %i; " s x)

let id x = x |> inc_log |> dec_log_upgraded

let log (name: string) (f: int -> int) : int -> int * string =
  fun x -> (f x, Printf.sprintf "Called %s on %i; " name x)
  
let loggable (name: string) (f: int -> int) : int * string -> int * string =
  fun (x, s1) ->
    let (y, s2) = log name f x in
    (y, s1 ^ s2)

let inc' : int * string -> int * string =
  loggable "inc" inc
  
let dec' : int * string -> int * string =
  loggable "dec" dec

(* let id' : int * string -> int * string =
  fun (x: int * string) -> inc' (dec' x) *)


let _ = dec' (inc' (5, ""))

let return (x : int) : int * string = (x, "")

let ( >>= ) (m : int * string) (f : int -> int * string) : int * string =
  let (x, s1) = m in
  let (y, s2) = f x in
  (y, s1 ^ s2)
  
let loggable (name : string) (f : int -> int) : int * string -> int * string =
  fun m ->
    m >>= fun x ->
    log name f x >>= fun y ->
    return y

module Writer (* : Monad *) = struct
  type 'a t = 'a * string
  let return x = (x, "")
  let ( >>= ) m f =
    let (x, s1) = m in
    let (y, s2) = f x in
    (y, s1 ^ s2)
end

let compose f g x =
  f x >>= fun y ->
  g y

let ( >=> ) = compose