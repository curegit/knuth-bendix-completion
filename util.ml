(* ユーティリティ *)

module TermRewritingSystemUtility = struct

  let rec map f = function
                  | [] -> []
                  | x :: xs -> f x :: map f xs

  let rec filter p = function
                     | [] -> []
                     | x :: xs -> if p x then x :: filter p xs else filter p xs

  let rec notwhere p = function
                       | [] -> []
                       | x :: xs -> if p x then notwhere p xs else x :: notwhere p xs

  let rec member = function
                   | [] -> fun x -> false
                   | y :: ys -> fun x -> if x = y then true else member ys x

  let rec find key = function
                     | [] -> None
                     | (k, v) :: vs -> if key = k then Some v else find key vs

  let rec append xs = function
                      | [] -> Some xs
                      | (k, v) :: ys -> match find k xs with
                                        | Some x -> if x = v then append xs ys else None
                                        | None -> match append xs ys with
                                                  | Some zs -> Some ((k, v) :: zs)
                                                  | None -> None

  let union xs ys = xs @ notwhere (member xs) ys

end
