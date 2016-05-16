{-y agregando el teorema 3.5 al modulo de teoremas con el enunciado   
(p <==> q) <==> q === p podemos hacer la siguiente demostración.
-}
import Funciones
import Estructuras
verify = let theorem = p === (q <==> q) <==> p in
         proof theorem
         >>=
         statement 3.5 with (p =: p) using lambda z (z)
         >>=
         statement 3.2 with (p =: p) using lambda z (z <==> q)
         >>=
         statement 3.1 with (q, p, q =: p, q, r) using lambda z (z)
         >>=
         statement 3.2 with (p =: p) using lambda z (q <==> z)
         >>=
         statement 3.1 with (p, q =: p, r) using lambda z (z)
         >>=
         done theorem