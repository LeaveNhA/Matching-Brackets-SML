fun isEmpty (xs : 'a list) =
    case xs of
        [] => true
      | _ => false

fun isBalancedPrime(s : char, xs : char list) =
    case (s, xs) of
        (* Collecting: *)
        (#"[", _) => [#"["] @ xs
      | (#"(", _) => [#"("] @ xs
      | (#"{", _) => [#"{"] @ xs
      (* Correcting: *)
      | (#"]", #"["::rest) => rest
      | (#")", #"("::rest) => rest
      | (#"}", #"{"::rest) => rest
      (* Failing: *)
      | (#"]", xs) => [#"]"] @ xs
      | (#")", xs) => [#")"] @ xs
      | (#"}", xs) => [#"}"] @ xs
      | _ => xs

fun isBalanced s =
    (isEmpty o (foldl (isBalancedPrime) []) o String.explode)(s)
