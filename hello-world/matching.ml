let x = 
  match not true with
  | true -> "nope"
  | false -> "yep"

let y =
  match 42 with
  | foo -> foo

let z = 
  match "foo" with
  | "bar" -> 0
  | _ -> 1

let a =
  match [] with 
  | [] -> "empty"
  | _ -> "not empty"

let b =
  match [1; 2] with 
  | [] -> "empty"
  | _ -> "not empty"

let c = 
  match ["ocaml"; "rust"] with
    | [] -> "not at all"
    | h :: t -> h
    
let d = 
  match ["ocaml"; "rust"] with
    | [] -> ["not at all"]
    | h :: t -> t

let fst3 t=
  match t with
  | (a, b, c) -> a

let first = fst3 (1, 2, 3)

type student = {
  name: string;
  year: int;
}
let rgb = {
  name = "Ruth Bader";
  year = 1954;
}

let name_with_year s =
  match s with 
    {name; year} -> name ^ " '" ^ string_of_int (year mod 100)

let ar = name_with_year rgb
  