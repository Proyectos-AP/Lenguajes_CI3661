{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Funciones 
(sust,
instantiate,
leibniz,
infer,
step,
compareEquation,
statement,
proof,
done
) where

{-
* Universidad Simón Bolívar
* Departamento de Computación y Tecnología de la Información
* Laboratorio de Lenguajes de Programación I (CI3661)
* 
* Archivo: Funciones.hs
*
* Descripción: 
* 
* Integrantes:
* 	Alejandra Cordero / 12-10645
*	Pablo Maldonado / 12-10561
*
* Referencias:
*
* Última modificación: 21/05/2016
*
-}

-- Importes: 
import Theorems
import Estructuras

class Sust a where
	sust :: Term -> a -> Term
	showSustitution :: a -> String

instance Sust Sust' where
	sust (Var s1) (Sustitution term2 (Var s2)) = if s1 == s2 then term2 else (Var s1)
	sust (Bool s1) susExpr = Bool s1
	sust (Or term1 term2) susExpr = Or (sust term1 susExpr) (sust term2 susExpr)
	sust (And term1 term2) susExpr = And (sust term1 susExpr) (sust term2 susExpr)
	sust (Impl term1 term2) susExpr = Impl (sust term1 susExpr) (sust term2 susExpr)
	sust (Equiv term1 term2) susExpr = Equiv (sust term1 susExpr) (sust term2 susExpr)
	sust (NoEquiv term1 term2) susExpr = NoEquiv (sust term1 susExpr) (sust term2 susExpr)

	showSustitution (Sustitution t1 t2) =  showTerm t1 ++ "=:" ++ showTerm t2

instance Sust (Term,Sust',Term) where
	sust (Var x1) (t1,Sustitution t2 (Var x2),(Var x3)) 
		| x1 == x2 = t1
		| x1 == x3 = t2
		| otherwise = (Var x1)

	sust (Bool x1) (t1,susExpr,x3) = Bool x1
	sust (Or term1 term2) (t1,susExpr,x3)  = Or (sust term1 (t1,susExpr,x3) ) (sust term2 (t1,susExpr,x3) )
	sust (And term1 term2) (t1,susExpr,x3) = And (sust term1 (t1,susExpr,x3)) (sust term2 (t1,susExpr,x3) )
	sust (Impl term1 term2) (t1,susExpr,x3) = Impl (sust term1 (t1,susExpr,x3)) (sust term2 (t1,susExpr,x3))
	sust (Equiv term1 term2) (t1,susExpr,x3) = Equiv (sust term1 (t1,susExpr,x3) ) (sust term2 (t1,susExpr,x3) )
	sust (NoEquiv term1 term2) (t1,susExpr,x3) = NoEquiv (sust term1 (t1,susExpr,x3) ) (sust term2 (t1,susExpr,x3) )

	showSustitution (t1,susExpr,x3) =  "("++ showTerm t1 ++
		"," ++ showSustitution susExpr ++ "," ++showTerm x3++")"

instance Sust (Term,Term,Sust',Term,Term) where
	sust (Var x1) (t1,t3,Sustitution t2 (Var x2),(Var x3),(Var x4)) 
		| x1 == x2 = t1
		| x1 == x3 = t3
		| x1 == x4 = t2
		| otherwise = (Var x1)

	sust (Bool x1) (t1,t3,susExpr,x3,x4)  = Bool x1
	sust (Or term1 term2) (t1,t3,susExpr,x3,x4)    = Or (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))
	sust (And term1 term2) (t1,t3,susExpr,x3,x4)   = And (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))
	sust (Impl term1 term2) (t1,t3,susExpr,x3,x4)  = Impl (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))
	sust (Equiv term1 term2) (t1,t3,susExpr,x3,x4) = Equiv (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))
	sust (NoEquiv term1 term2) (t1,t3,susExpr,x3,x4)  = NoEquiv (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))

	showSustitution (t1,t3,susExpr,x3,x4) =  "("++ showTerm t1 ++"," ++ showTerm t3 ++
	 "," ++ showSustitution susExpr ++ "," ++ showTerm x3 ++","++ showTerm x4++")"

-- instantiate:
instantiate :: (Sust a) => Equation -> a -> Equation
instantiate (Equa t1 t2) susExpr = Equa (sust t1 susExpr) (sust t2 susExpr)

-- leibniz:
leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equa t1 t2) var expr = Equa (sust expr (t1=:var)) (sust expr (t2=:var))

-- infer:
infer :: (Sust a) =>  Float -> a -> Term -> Term -> Equation
infer n s var expr = leibniz (instantiate (prop n) s) var (expr)

-- step:
step :: (Sust a) => Term -> Float -> a -> Term -> Term -> Term
step term1 n susExpr var expr = compareEquation term1 (infer n susExpr var expr)

-- compareEquation: 
compareEquation :: Term -> Equation -> Term
compareEquation term1 (Equa t1 t2) 
	| term1==t1 = t2
	| term1==t2 = t1
	| otherwise = error "*** No se puede seguir instanciando ***"


-- statement:
statement :: (Sust a) =>  Float -> Dummy -> a -> Dummy -> Dummy -> Term -> Term -> Term -> IO Term
statement n with susExpr using lambda var expr term1  = 
	do{ 
	putStrLn ("=== statement "++ show n++ " with " ++ showSustitution susExpr ++ 
		" using lambda "++show var++"."++show expr);
	putStrLn (show (step term1 n susExpr var expr) );
	return (step term1 n susExpr var expr)
	}

-- proof:
proof :: Equation -> IO Term
proof (Equa t1 t2) = do { putStrLn ("Prooving "++ show (Equa t1 t2) ); putStrLn (show t1); return t1}

-- done:
done :: Equation -> Term -> IO ()
done (Equa t1 t2) term1 = do { if term1==t2 then putStrLn "Proof succesful." else putStrLn "Proof unsuccesful."}

-- showTerm:
showTerm :: Term -> String

-- Variables y Booleanos
showTerm (Var x) = x
showTerm (Bool x) = x

-- Operación: Negación neg
showTerm (Not (Var x)) = "neg " ++ showTerm(Var x)
showTerm (Not (Bool x)) = "neg " ++ showTerm (Bool x)
showTerm (Not t) = "neg " ++ "(" ++  showTerm t ++ ")"

-- Operación: Or \/ 
showTerm (Or (Var x) (Var y)) = showTerm(Var x) ++ " \\/ " ++ showTerm(Var y)
showTerm (Or (Var x) (Bool y)) = showTerm(Var x) ++ " \\/ " ++ showTerm(Bool y)
showTerm (Or (Bool x) (Var y)) = showTerm(Bool x) ++ " \\/ " ++ showTerm(Var y)
showTerm (Or (Var x) t) = showTerm(Var x) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm (Or t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var x)
showTerm (Or (Bool x) (Bool y)) = showTerm(Bool x) ++ "\\/" ++ showTerm(Bool y)
showTerm (Or (Bool x) t) = showTerm(Bool x) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm (Or t (Bool x)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Bool x)
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"

-- Operación: And /\
showTerm (And (Var x) (Var y)) = showTerm(Var x) ++ " /\\ " ++ showTerm(Var y)
showTerm (And (Var x) (Bool y)) = showTerm(Var x) ++ " /\\ " ++ showTerm(Bool y)
showTerm (And (Bool x) (Var y)) = showTerm(Bool x) ++ " /\\ " ++ showTerm(Var y)
showTerm (And (Var x) t) = showTerm(Var x) ++ " /\\ (" ++ showTerm(t) ++ ")"
showTerm (And t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " /\\ " ++ showTerm(Var x)
showTerm (And (Bool x) (Bool y)) = showTerm(Bool x) ++ "/\\" ++ showTerm(Bool y)
showTerm (And (Bool x) t) = showTerm(Bool x) ++ " /\\ (" ++ showTerm(t) ++ ")"
showTerm (And t (Bool x)) = "(" ++ showTerm(t) ++ ")" ++ " /\\ " ++ showTerm(Bool x)
showTerm (And t1 t2) = "(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"

-- Operación: Implicación ==>
showTerm (Impl (Var x) (Var y)) = showTerm(Var x) ++ " ==> " ++ showTerm(Var y)
showTerm (Impl (Var x) (Bool y)) = showTerm(Var x) ++ " ==> " ++ showTerm(Bool y)
showTerm (Impl (Bool x) (Var y)) = showTerm(Bool x) ++ " ==> " ++ showTerm(Var y)
showTerm (Impl (Var x) t) = showTerm(Var x) ++ " ==> (" ++ showTerm(t) ++ ")"
showTerm (Impl t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " ==> " ++ showTerm(Var x)
showTerm (Impl (Bool x) (Bool y)) = showTerm(Bool x) ++ "==>" ++ showTerm(Bool y)
showTerm (Impl (Bool x) t) = showTerm(Bool x) ++ " ==> (" ++ showTerm(t) ++ ")"
showTerm (Impl t (Bool x)) = "(" ++ showTerm(t) ++ ")" ++ " ==> " ++ showTerm(Bool x)
showTerm (Impl t1 t2) = "(" ++ showTerm t1 ++ ") ==> (" ++ showTerm t2 ++ ")"

-- Operación: Equivalencia <==>
showTerm (Equiv (Var x) (Var y)) = showTerm(Var x) ++ " <==> " ++ showTerm(Var y)
showTerm (Equiv (Var x) (Bool y)) = showTerm(Var x) ++ " <==> " ++ showTerm(Bool y)
showTerm (Equiv (Bool x) (Var y)) = showTerm(Bool x) ++ " <==> " ++ showTerm(Var y)
showTerm (Equiv (Var x) t) = showTerm(Var x) ++ " <==> (" ++ showTerm(t) ++ ")"
showTerm (Equiv t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " <==> " ++ showTerm(Var x)
showTerm (Equiv (Bool x) (Bool y)) = showTerm(Bool x) ++ "<==>" ++ showTerm(Bool y)
showTerm (Equiv (Bool x) t) = showTerm(Bool x) ++ " <==> (" ++ showTerm(t) ++ ")"
showTerm (Equiv t (Bool x)) = "(" ++ showTerm(t) ++ ")" ++ " <==> " ++ showTerm(Bool x)
showTerm (Equiv t1 t2) = "(" ++ showTerm t1 ++ ") <==> (" ++ showTerm t2 ++ ")"

-- Operación: Inequivalencia !<==>
showTerm (NoEquiv (Var x) (Var y)) = showTerm(Var x) ++ " !<==> " ++ showTerm(Var y)
showTerm (NoEquiv (Var x) (Bool y)) = showTerm(Var x) ++ " !<==> " ++ showTerm(Bool y)
showTerm (NoEquiv (Bool x) (Var y)) = showTerm(Bool x) ++ " !<==> " ++ showTerm(Var y)
showTerm (NoEquiv (Var x) t) = showTerm(Var x) ++ " !<==> (" ++ showTerm(t) ++ ")"
showTerm (NoEquiv t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " !<==> " ++ showTerm(Var x)
showTerm (NoEquiv (Bool x) (Bool y)) = showTerm(Bool x) ++ "!<==>" ++ showTerm(Bool y)
showTerm (NoEquiv (Bool x) t) = showTerm(Bool x) ++ " !<==> (" ++ showTerm(t) ++ ")"
showTerm (NoEquiv t (Bool x)) = "(" ++ showTerm(t) ++ ")" ++ " !<==> " ++ showTerm(Bool x)
showTerm (NoEquiv t1 t2) = "(" ++ showTerm t1 ++ ") !<==> (" ++ showTerm t2 ++ ")"

instance Show Term where show = showTerm

showEquation :: Equation -> String
showEquation (Equa t1 t2) = showTerm t1 ++ " === " ++ showTerm t2 
instance Show Equation where show = showEquation

{-showSustitution :: Sust' -> String
showSustitution (Sustitution t1 t2) =  showTerm t1 ++ "=:" ++ showTerm t2
instance Show Sust' where show = showSustitution

-}

