type student = {
  name: string;
  year: int; (* grad year *)
}

let rbg = {
  name = "Ruth Bader";
  year = 1954;
}

let r = {rbg with name = "Ruth Bader Ginsburg"}