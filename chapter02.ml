open Base

(*
   - [Base] is a lightweight and portable standard library replacement.
   - [Core] extends [Base] with data structures such as:
   - heaps
   - hash-sets
   - functional queues
     It also provides types such as time/timezone related.

   It takes longer to build and will increase the executable size.
*)

let%test_unit "basic arithmetic" =
  ()
  ; [%test_eq: int] 7 (3 + 4)
  ; [%test_eq: int] 4 (12 / 3)
  ; [%test_eq: int] 4 (13 / 3)
  ; [%test_eq: float] 4. (12. /. 3.)
  ; [%test_eq: float] 4.333333333333333 (13. /. 3.)
;;

let%test_unit "A nicer way to work with floats" =
  (* This module is designed to be onpened *)
  let open Float.O in
  [%test_eq: float] 4. (12. / 3.)
;;

let%test_unit "Alternatively, use the more precise local open" =
  let x = Float.O.(12.0 / 3.0 * 100.0) in
  [%test_eq: float] 400.0 x
;;

let%test_unit "The more common modulo operator is available" =
  ()
  ; [%test_eq: int] 0 (0 % 3)
  ; [%test_eq: int] 1 (1 % 3)
  ; [%test_eq: int] 2 (2 % 3)
  ; [%test_eq: int] 0 (3 % 3)
;;

(* Skipping a bunch of stuff I already know... *)

(* I think the  term "inductive" refers to recursion having a base case (as
   oppose to other forms of recursion, such as infinite recursion).

   More info here:
   https://cs3110.github.io/textbook/chapters/correctness/structural_induction.html
*)
let rec sum = function
  | [] -> 0             (* base case *)
  | h :: t -> h + sum t (* inductive case *)
[@@ocamlformat "disable"]
