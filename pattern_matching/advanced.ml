type ptype = TNormal | TFire | TWater

type peff = ENormal | ENotVery | ESuper

let mult_off_eff = function
  | ENormal -> 1.
  | ENotVery -> 0.5
  | ESuper -> 2.0

let eff = function
  | (TFire, TFire) -> ENotVery
  | (TWater, TWater) -> ENotVery
  | (TFire, TWater) -> ENotVery
  | (TWater, TFire) -> ESuper
  | _ -> ENormal

let eff_bet = function
  | (TFire, TFire) -> ENotVery | (TWater, TWater) -> ENotVery  | (TFire, TWater)
    -> ENotVery
  | (TWater, TFire) -> ESuper
  | _ -> ENormal

let eff_2 t1 t2 = match t1, t2 with
  | TFire, TFire -> ENotVery | TWater, TWater -> ENotVery  | TFire, TWater
  -> ENotVery
  | TWater, TFire -> ESuper
  | _ -> ENormal
  
type mon = {
  name: string;
  hp: int;
  ptype: ptype;
}

let charmander = {
  name = "Charmander";
  hp = 39;
  ptype = TFire;
}