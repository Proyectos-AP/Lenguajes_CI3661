data Term = Var String | Bool String | Or Term Term | And Term Term | Impl Term Term | Equiv Term Term | NoEquiv Term Term 
data Equation = Equa Term Term
data Sust = Sustitution Term Term | Tuple (Term,Sust,Term) | Tuples (Term,Term,Sust,Term,Term)

a :: Term
a = Var "a"

b :: Term
b = Var "b"

c :: Term
c = Var "c"

d :: Term
d = Var "d"

e :: Term
e = Var "e"

f :: Term
f = Var "f"

g :: Term
g = Var "g"

h :: Term
h = Var "h"

i :: Term
i = Var "i"

j :: Term
j = Var "j"

k :: Term
k = Var "k"

l :: Term
l = Var "l"

m :: Term
m = Var "m"

n :: Term
n = Var "n"

o :: Term
o = Var "o"

p :: Term
p = Var "p"

q :: Term
q = Var "q"

r :: Term
r = Var "r"

s :: Term
s = Var "s"

t :: Term
t = Var "t"

u :: Term
u = Var "u"

v :: Term
v = Var "v"

w :: Term
w = Var "w"

x :: Term
x = Var "x"

y :: Term
y = Var "y"

z :: Term
z = Var "z"

true :: Term
true = Bool "true"

false :: Term
false =  Bool "false"

(\/) :: Term -> Term -> Term
(\/) t1 t2 = Or t1 t2

(/\) :: Term -> Term -> Term
(/\) t1 t2 = And t1 t2

infixr 8 ==>
(==>) :: Term -> Term -> Term
(==>) t1 t2 = Impl t1 t2

(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = Equiv t1 t2

(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = NoEquiv t1 t2

infix 2 ===
(===) :: Term -> Term -> Equation
(===) t1 t2 = Equa t1 t2

infix 2 =:
(=:) :: Term -> Term -> Sust
(=:) t1 (Var t2) = Sustitution t1 (Var t2)


sust :: Term -> Sust -> Term
sust (Var s1) (Sustitution term2 (Var s2)) = if s1 == s2 then term2 else (Var s1)
sust (Bool s1) (Sustitution term2 (Var s2)) = Bool s1
sust (Or term1 term2) (Sustitution term3 s2) = Or (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))
sust (And term1 term2) (Sustitution term3 s2) = And (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))
sust (Impl term1 term2) (Sustitution term3 s2) = Impl (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))
sust (Equiv term1 term2) (Sustitution term3 s2) = Equiv (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))
sust (NoEquiv term1 term2) (Sustitution term3 s2) = NoEquiv (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))


instantiate :: Equation -> Sust -> Equation
instantiate (Equa t1 t2) (Sustitution t3 (Var s2)) = Equa (sust t1 (Sustitution t3 (Var s2))) (sust t2 (Sustitution t3 (Var s2)))

 
leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equa t1 t2) var expr = Equa (sust expr (t1=:var)) (sust expr (t2=:var))




{-sust :: Term -> Sust -> Term (Listo)
instantiate :: Equation -> Sust -> Equation (Listo)
leibniz :: (Listo)
infer :: Int -> Equation -> Sust -> z -> E
step :: Term -> Int -> Equation -> Sust -> z -> E -> Term
with::
using::
lambda::-}

{-i = \x -> x

k = \x -> \y -> x

s = \x -> \y -> \z -> (x z) (y z)

abstraer :: Term -> Term -> (Term -> Term)
abstraer (Var x) (Var y) = if x == y then i else k (Var x)
abstraer (Var x) (Or t1 t2) = s (s (k Or) (abstraer (Var x) t1)) (abstraer (Var x) t2) 
-}
showTerm :: Term -> String
showTerm (Var x) = x
showTerm (Bool x) = x
showTerm (Or (Var x) (Var y)) = showTerm(Var x) ++ "\\/" ++ showTerm(Var y)
showTerm (Or (Var x) t) = showTerm(Var x) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm (Or t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var x)
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"
showTerm (And (Var x) (Var y)) = showTerm(Var x) ++ "/\\" ++ showTerm(Var y)
showTerm (And (Var x) t) = showTerm(Var x) ++ " /\\ (" ++ showTerm(t) ++ ")"
showTerm (And t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " /\\ " ++ showTerm(Var x)
showTerm (And t1 t2) = "(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"
showTerm (Impl (Var x) (Var y)) = showTerm(Var x) ++ "==>" ++ showTerm(Var y)
showTerm (Impl (Var x) t) = showTerm(Var x) ++ " ==> (" ++ showTerm(t) ++ ")"
showTerm (Impl t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " ==> " ++ showTerm(Var x)
showTerm (Impl t1 t2) = "(" ++ showTerm t1 ++ ") ==> (" ++ showTerm t2 ++ ")"
showTerm (Equiv (Var x) (Var y)) = showTerm(Var x) ++ "<==>" ++ showTerm(Var y)
showTerm (Equiv (Var x) t) = showTerm(Var x) ++ " <==> (" ++ showTerm(t) ++ ")"
showTerm (Equiv t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " <==> " ++ showTerm(Var x)
showTerm (Equiv t1 t2) = "(" ++ showTerm t1 ++ ") <==> (" ++ showTerm t2 ++ ")"
showTerm (NoEquiv (Var x) (Var y)) = showTerm(Var x) ++ "!<==>" ++ showTerm(Var y)
showTerm (NoEquiv (Var x) t) = showTerm(Var x) ++ " !<==> (" ++ showTerm(t) ++ ")"
showTerm (NoEquiv t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " !<==> " ++ showTerm(Var x)
showTerm (NoEquiv t1 t2) = "(" ++ showTerm t1 ++ ") !<==> (" ++ showTerm t2 ++ ")"
instance Show Term where show = showTerm

showEquation :: Equation -> String
showEquation (Equa t1 t2) = showTerm t1 ++ " === " ++ showTerm t2 
instance Show Equation where show = showEquation

showSustitution :: Sust -> String
showSustitution (Sustitution t1 t2) =  showTerm t1 ++ "=:" ++ showTerm t2
instance Show Sust where show = showSustitution



