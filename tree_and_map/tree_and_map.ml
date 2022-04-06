type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let t = 
  Node (2,
        Node(1, Leaf, Leaf),
        Node(3, Leaf, Leaf)
        )

let rec map f = function 
  | Leaf -> Leaf
  | Node (v, l, r) -> Node(f v, map f l, map f r)

let add1 t = map succ t

let t1 = add1 t

let rec fold acc f = function
  | Leaf -> acc
  | Node (v, l, r) -> f v (fold acc f l) (fold acc f r)

let sum t = fold 0 (fun x y z -> x + y + z) t

let sumt = sum t

let rec filter p = function
  | Leaf -> Leaf
  | Node (v, l, r) -> if p v then Node(v, filter p l, filter p r) else Leaf

let even x = x mod 2 == 0

let oddt = filter even t