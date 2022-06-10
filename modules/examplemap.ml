module IntMap = Map.Make(Int)

open IntMap
let m1 = add 1 "one" empty
let _ = find 1 m1
let _ = bindings(m1)

let m2 = add 1 1. empty
let _ = bindings m2

type name = {first: string; last: string}

module Name = struct
  type t = name
  let compare {first = first1; last = last1} {first = first2; last = last2}
    =
    match String.compare last1 last2 with
    | 0 -> String.compare first1 first2
    | c -> c
end

module NameMap = Map.Make(Name)

let k1 = {last = "Kardashian"; first = "Kourtney"}
let k2 = {last = "Kardashian"; first = "Kimberly"}
let k3 = {last = "Kardashian"; first = "Khloe"}
let k4 = {last = "West"; first = "Kanye"}

let nm =
  NameMap.(empty |> add k1 1979 |> add k2 1980 |> add k3 1984 |> add k4 1977)

let lst = NameMap.bindings nm