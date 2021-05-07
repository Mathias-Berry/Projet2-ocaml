let rec f x = match x with
    | [] -> 0
    | t::q -> f (q+1)
in f
