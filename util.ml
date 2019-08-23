let rec map f = function
                | [] -> []
                | x :: xs -> f x :: map f xs

let rec filter p = function
                   | [] -> []
                   | x :: xs -> if p x then x :: filter p xs else filter p xs

let rec notwhere p = function
                     | [] -> []
                     | x :: xs -> if p x then notwhere p xs else x :: notwhere p xs

let rec fold f a = function
                   | [] -> a
                   | x :: xs -> fold f (f a x) xs

let rec all p = function
                | [] -> true
                | x :: xs -> if p x then all p xs else false

let rec any p = function
                | [] -> false
                | x :: xs -> if p x then true else any p xs

let rec member = function
                 | [] -> fun _ -> false
                 | y :: ys -> fun x -> if x = y then true else member ys x

let rec find key = function
                   | [] -> None
                   | (k, v) :: vs -> if key = k then Some v else find key vs

let rec append xs = function
                    | [] -> Some xs
                    | (k, v) as y :: ys -> match find k xs with
                                           | Some x -> if x = v then append xs ys else None
                                           | None -> match append xs ys with
                                                     | Some zs -> Some (y :: zs)
                                                     | None -> None

let union xs ys = xs @ notwhere (member xs) ys

let substraction xs ys = notwhere (member ys) xs

let intersection xs ys = filter (member ys) xs

let reverse xs = let rec inner = function
                                 | [] -> fun r -> r
                                 | x' :: xs' -> fun r -> inner xs' (x' :: r)
                     in inner xs []

let some = function
           | Some _ -> true
           | None -> false

let sgn = function
          | n when n > 0 -> 1
          | n when n < 0 -> -1
          | _ -> 0
