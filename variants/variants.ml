type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
let d = Tue

let int_of_day d = 
  match d with 
  | Sun -> 1
  | Mon -> 2
  | Tue -> 3
  | Wed -> 4
  | Thu -> 5
  | Fri -> 6
  | Sat -> 7

type primary_color = Red | Green | Blue

let r = Red

type point = float * float
type shapes = 
  | Circle of {center: point; radius: float}
  | Rectangle of {lower_left: point; upper_right: point}
  | Point of point

let c1 = Circle {center = (0., 0.); radius = 1.}
let r1 = Rectangle {lower_left = (-1., -1.); upper_right = (1., 1.)}
let p1 = Point (31., 10.)

let avg a b =
  (a +. b) /. 2.

let center s = 
  match s with
  | Circle {center; radius} -> center
  | Rectangle {lower_left; upper_right} -> 
    let (x_ll, y_ll) = lower_left in
    let (x_ur, y_ur) = upper_right in
    (avg x_ll x_ur, avg y_ll y_ur)
  | Point (x, y) -> (x, y)

let center2 s = 
  match s with
  | Circle {center; radius} -> center
  | Rectangle {lower_left=(x_ll, y_ll); upper_right=(x_ur, y_ur)} -> 
    (avg x_ll x_ur, avg y_ll y_ur)
  | Point p -> p