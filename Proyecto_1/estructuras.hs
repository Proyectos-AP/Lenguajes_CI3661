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

(==>) :: Term -> Term -> Term
(==>) t1 t2 = Impl t1 t2

(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = Equiv t1 t2

(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = NoEquiv t1 t2

(===) :: Term -> Term -> Equation
(===) t1 t2 = Equa t1 t2

(=:) :: Term -> Term -> Sust
(=:) t1 t2 = Sustitution t1 t2

{-sust :: Term -> Sust -> Term
instantiate :: Equation -> Sust -> Equation
leibniz ::
infer :: Int -> Equation -> Sust -> z -> E
step :: Term -> Int -> Equation -> Sust -> z -> E -> Term
with::
using::
lambda::-}

showTerm :: Term -> String
showTerm (Var x) = x
showTerm (Bool x) = x
instance Show Term where show = showTerm
