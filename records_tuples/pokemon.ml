type ptype = TNormal | TFire | TWater
type mon = {name : string; hp : int; ptype : ptype}

let ch = {name = "Charmander"; hp = 39; ptype = TFire}

let h = match ch with {name = n; hp = h; ptype = t} -> h
