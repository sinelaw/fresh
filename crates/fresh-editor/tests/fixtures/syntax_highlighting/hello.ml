(* OCaml syntax highlighting test *)
let greet name =
  Printf.sprintf "Hello, %s!" name

let factorial n =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (acc * n) (n - 1)
  in
  aux 1 n

let () =
  let message = greet "World" in
  print_endline message;
  Printf.printf "Factorial 10 = %d\n" (factorial 10)
