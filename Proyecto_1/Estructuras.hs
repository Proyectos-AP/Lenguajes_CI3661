module Estructuras 
(Term (Var,Bool,Or,And,Impl,Equiv,NoEquiv,Not),
Equation (Equa), 
Sust' (Sustitution),
Dummy (With,Lambda,Using),
a,
b,
c,
d,
e,
f,
g,
h,
i,
j,
k,
l,
m,
n,
o,
p,
q,
r,
s,
t,
u,
v,
w,
x,
y,
z,
true,
false,
using,
with,
lambda,
neg,
(<==>),
(===),
(==>),
(\/),
(/\),
(!<==>),
(=:)
) where

{-
* Universidad Simón Bolívar
* Departamento de Computación y Tecnología de la Información
* Laboratorio de Lenguajes de Programación I (CI3661)
* 
* Archivo: Estructuras.hs
*
* Descripción: 
* 
* Integrantes:
* 	Alejandra Cordero / 12-10645
*	Pablo Maldonado / 12-10561
*
* Última modificación: 21/05/2016
*
-}


data Term = Var String | Bool String | Or Term Term | And Term Term | Impl Term Term | Equiv Term Term | NoEquiv Term Term | Not Term deriving (Eq) 
data Equation = Equa Term Term
data Sust' = Sustitution Term Term 
data Dummy = With String | Lambda String | Using String

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

using :: Dummy
using = Using "using"

with :: Dummy
with = With "with"

lambda :: Dummy
lambda = Lambda "lambda"

{-((,,)) :: Term -> Sust -> Term
((,,)) t1 s t2 = Tuple (t1,s,t2)-}


neg :: Term -> Term
neg t1 =  Not t1

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
(=:) :: Term -> Term -> Sust'
(=:) t1 (Var t2) = Sustitution t1 (Var t2)

