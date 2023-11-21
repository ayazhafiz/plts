# cor +solve -elab
List a : [ Nil, Cons a (List a) ]

sig map : (a -> b) -> List a -> List b
#   ^^^
let map = \f -> \xs ->
  let rec go = \xs ->
    when xs is
#        ^^
      | Nil -> Nil
      | Cons x xs -> Cons (f x) (go xs)
    end
  in go xs
;;

let mapper = \x -> A x;;
#   ^^^^^^

run main = map mapper (Cons 1 (Cons 2 Nil));;

> cor-out +solve -elab
> 
> List a : [ Nil, Cons a (List a) ]
> 
> sig map : (a -> b) -> List a -> List b
> #   ^^^ ('a -> 'b) -> %(List 'a1) -> %List 'b1
> let map = \f -> \xs ->
>   let rec go = \xs ->
>     when xs is
>       | Nil -> Nil
>       | Cons x xs -> Cons (f x) (go xs)
>     end
>   in go xs
> ;;
> 
