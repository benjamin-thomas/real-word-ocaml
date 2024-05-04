open Base

(*{|
     - [Base] is a lightweight and portable standard library replacement.
     - [Core] extends [Base] with data structures such as:
        - heaps
        - hash-sets
        - functional queues
          It also provides types such as time/timezone related.

        It takes longer to build and will increase the executable size.
  |}*)

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

(* 2.3.2 – Lists *)

(* I think the  term "inductive" refers to recursion having a base case (as
   oppose to other forms of recursion, such as infinite recursion).

   More info here:
   https://cs3110.github.io/textbook/chapters/correctness/structural_induction.html
*)
let rec sum = function
  | [] -> 0             (* base case *)
  | h :: t -> h + sum t (* inductive case *)
[@@ocamlformat "disable"]

let%test_unit "sum" = [%test_eq: int] 10 (sum [ 1; 2; 3; 4 ])

(** [rm_seq_dups] removes sequential duplicates *)
let rec rm_seq_dups = function
  | [] -> []
  | [ x ] -> [ x ]
  | a :: b :: rest ->
    if a = b then
      a :: rm_seq_dups rest
    else
      a :: rm_seq_dups (b :: rest)
;;

let%test_unit "rm_seq_dups" =
  ()
  ; [%test_eq: int list] [] (rm_seq_dups [])
  ; [%test_eq: int list] [ 1; 2; 3; 4 ] (rm_seq_dups [ 1; 1; 2; 3; 3; 4 ])
;;

(* 2.3.3 – Options *)

let safe_div x y =
  if y = 0 then
    None
  else
    Some (x / y)
;;

let safe_div' opt_x y = Option.bind ~f:(fun x -> safe_div x y) opt_x

let%test_unit "safe div" =
  let ( / ) = safe_div' in
  ()
  ; [%test_result: int option] ~expect:(Some 42) (Some 1024 / 4 / 3 / 2 / 1)
  ; [%test_result: int option] ~expect:None (Some 1024 / 4 / 3 / 2 / 1 / 0)
;;

let downcase_extension filename =
  match String.rsplit2 filename ~on:'.' with
  | None -> filename
  | Some (base, ext) -> base ^ "." ^ String.lowercase ext
;;

let%test_unit "extension" =
  ()
  ; [%test_eq: string list]
      [ "FOO"; "Bar"; "FOO.bar"; "Foo.baz" ]
      (List.map ~f:downcase_extension [ "FOO"; "Bar"; "FOO.BAR"; "Foo.BaZ" ])
;;

(* 2.4 – Records and Variants *)

(* Nothing of interest, appart from the fact that one must open a module to
   compare values of a given type.contents

   NOTE: ( = ) only works on ints.
*)

type shape =
  | Circle of { radius : float }
  | Square of
      { width : int
      ; height : int
      }
[@@deriving equal]

module Shape = struct
  type t = shape [@@deriving equal]

  let ( = ) = equal
end

let%test_unit "compare" =
  let sq1 = Square { width = 1; height = 1 } in
  let sq2 = Square { width = 1; height = 2 } in
  let ci1 = Circle { radius = 1.0 } in
  let ci2 = Circle { radius = 2.0 } in
  ()
  ; [%test_eq: bool] true (1 = 1)
  ; [%test_eq: bool] false (1 = 2)
  ; [%test_eq: bool] true Float.O.(0.1 = 0.1)
  ; [%test_eq: bool] false Float.O.(0.1 = 0.2)
  ; [%test_eq: bool] true (equal_shape sq1 sq1)
  ; [%test_eq: bool] false (equal_shape ci1 sq2)
  ; [%test_eq: bool] true Shape.(ci1 = ci1)
  ; [%test_eq: bool] false Shape.(ci1 = ci2)
;;

(* 2.5 – Imperative programming

   We may use imperative code rather than functional code. This could enable more
   efficient operations, requiring less memory.
*)

(** The fields in [running_sum] are designed to be easy to extend incrementall,
    and sufficient to compute means and standard deviations. *)
type running_sum =
  { mutable sum : float
  ; mutable sum_sq : float (* sum of squares *)
  ; mutable samples : int
  }

let mean rsum = rsum.sum /. Float.of_int rsum.samples

let stdev rsum =
  Float.sqrt ((rsum.sum_sq /. Float.of_int rsum.samples) -. (mean rsum **. 2.))
;;

(* We also need functions to create and update running_sums *)

let create () = { sum = 0.0; sum_sq = 0.0; samples = 0 }

let update rsum x =
  ()
  ; rsum.samples <- rsum.samples + 1
  ; rsum.sum <- rsum.sum +. x
  ; rsum.sum_sq <- rsum.sum_sq +. (x **. 2.)
;;

let%test_unit "running sum" =
  let rsum = create () in
  ()
  ; List.iter [ 1.; 3.; 2.; -7.; 4.; 5. ] ~f:(update rsum)
  ; [%test_eq: float] 1.33333333333333326 (mean rsum)  (* rwo *)
  ; [%test_eq: float] 3.94405318873307698 (stdev rsum) (* rwo *)
[@@ocamlformat "disable"]

(* 2.5.3 – Refs *)

(* There's nothing special about this type, we could define it ourseve (and it's operators) *)

type 'a ref = { mutable contents : 'a }

let ref x = { contents = x }
let ( ! ) r = r.contents
let ( := ) r v = r.contents <- v

let%test_unit "ref" =
  let r = ref 1 in
  ()
  ; r := 2
  ; [%test_eq: int] 2 !r
;;

(* We can use refs to simulate the imperative style common in other languages. *)
let sum_list lst =
  let total = ref 0 in
  ()
  ; List.iter ~f:(fun x -> total := !total + x) lst
  ; !total
;;

let%test_unit "sum list" =
  let lst = [ 1; 2; 3; 4; 5 ] in
  [%test_eq: int] 15 (sum_list lst)
;;

(* 2.5.4 – For and While loops *)

let shuffle arr =
  let len = Array.length arr in
  ()
  ; for i = 0 to len - 2 do
      let j = Random.int (len - i) in
      let tmp = arr.(i) in
      ()
      ; arr.(i) <- arr.(j)
      ; arr.(j) <- tmp
    done
;;

(* NOTE:
   It seems that the random seed is static and can't be changed (easily) when under test.
   So it's okay to test random outputs.
*)
let%test_unit "permute array" =
  let arr = [| 1; 2; 3; 4; 5 |] in
  ()
  ; shuffle arr
  ; [%test_eq: int array] [| 4; 3; 5; 2; 1 |] arr
  ; shuffle arr
  ; [%test_eq: int array] [| 5; 3; 2; 4; 1 |] arr
  ; shuffle arr
  ; [%test_eq: int array] [| 3; 4; 1; 2; 5 |] arr
;;

(* While loops *)
let find_first_negative arr =
  let pos = ref 0 in
  ()
  ; while !pos < Array.length arr && arr.(!pos) >= 0 do
      pos := !pos + 1
    done
  ; if !pos = Array.length arr then
      None
    else
      Some arr.(!pos)
;;

let%test_unit "find first negative" =
  let arr1 = [| 1; 2; 3; 4; 5 |] in
  let arr2 = [| 1; 2; -3; 4; 5 |] in
  ()
  ; [%test_eq: int option] None (find_first_negative arr1)
  ; [%test_eq: int option] (Some (-3)) (find_first_negative arr2)
;;
