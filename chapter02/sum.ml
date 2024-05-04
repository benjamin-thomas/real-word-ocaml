open Base
module IC = Stdio.In_channel
module OC = Stdio.Out_channel

let rec read_and_accum acc =
  let line = IC.input_line IC.stdin in
  match line with
  | None -> acc
  | Some str -> read_and_accum (acc +. Float.of_string str)
;;

(* echo -e "1\n2\n3" | dune exec ./chapter02/sum.exe - *)
let () = OC.printf "Total: %F\n" (read_and_accum 0.)
