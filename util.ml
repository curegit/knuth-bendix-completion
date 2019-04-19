(* ユーティリティ *)

module TermRewritingSystemUtility = struct

  let rec member x = function
                     | [] -> false
                     | y :: ys -> if x = y then true else member x ys

  let rec union xs = function
                     | [] -> xs
                     | y :: ys -> if member y xs then xs else y :: xs

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

  let rec map f = function
                  | [] -> []
                  | x :: xs -> f x :: map f xs

end

