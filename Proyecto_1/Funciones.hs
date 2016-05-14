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

import Theorems
import Estructuras


class Sust' a where
	sust :: Term -> a -> Term

instance Sust' Sust where
	sust (Var s1) (Sustitution term2 (Var s2)) = if s1 == s2 then term2 else (Var s1)
	sust (Bool s1) (Sustitution term2 (Var s2)) = Bool s1
	sust (Or term1 term2) (Sustitution term3 s2) = Or (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))
	sust (And term1 term2) (Sustitution term3 s2) = And (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))
	sust (Impl term1 term2) (Sustitution term3 s2) = Impl (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))
	sust (Equiv term1 term2) (Sustitution term3 s2) = Equiv (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))
	sust (NoEquiv term1 term2) (Sustitution term3 s2) = NoEquiv (sust term1 (Sustitution term3 s2)) (sust term2 (Sustitution term3 s2))

instance Sust' (Term,Sust,Term) where
	sust (Var x1) (t1,Sustitution t2 (Var x2),(Var x3)) 
		| x1 == x2 = t2
		| x1 == x3 = t1
		| otherwise = (Var x1)

	sust (Bool x1) (t1,susExpr,x3) = Bool x1
	sust (Or term1 term2) (t1,susExpr,x3)  = Or (sust term1 (t1,susExpr,x3) ) (sust term2 (t1,susExpr,x3) )
	sust (And term1 term2) (t1,susExpr,x3) = And (sust term1 (t1,susExpr,x3)) (sust term2 (t1,susExpr,x3) )
	sust (Impl term1 term2) (t1,susExpr,x3) = Impl (sust term1 (t1,susExpr,x3)) (sust term2 (t1,susExpr,x3))
	sust (Equiv term1 term2) (t1,susExpr,x3) = Equiv (sust term1 (t1,susExpr,x3) ) (sust term2 (t1,susExpr,x3) )
	sust (NoEquiv term1 term2) (t1,susExpr,x3) = NoEquiv (sust term1 (t1,susExpr,x3) ) (sust term2 (t1,susExpr,x3) )


instance Sust' (Term,Term,Sust,Term,Term) where
	sust (Var x1) (t1,t3,Sustitution t2 (Var x2),(Var x3),(Var x4)) 
		| x1 == x2 = t2
		| x1 == x3 = t3
		| x1 == x4 = t1
		| otherwise = (Var x1)

	sust (Bool x1) (t1,t3,susExpr,x3,x4)  = Bool x1
	sust (Or term1 term2) (t1,t3,susExpr,x3,x4)    = Or (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))
	sust (And term1 term2) (t1,t3,susExpr,x3,x4)   = And (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))
	sust (Impl term1 term2) (t1,t3,susExpr,x3,x4)  = Impl (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))
	sust (Equiv term1 term2) (t1,t3,susExpr,x3,x4) = Equiv (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))
	sust (NoEquiv term1 term2) (t1,t3,susExpr,x3,x4)  = NoEquiv (sust term1 (t1,t3,susExpr,x3,x4)) (sust term2 (t1,t3,susExpr,x3,x4))


instantiate :: Equation -> Sust -> Equation
instantiate (Equa t1 t2) (Sustitution t3 (Var s2)) = Equa (sust t1 (Sustitution t3 (Var s2))) (sust t2 (Sustitution t3 (Var s2)))

 
leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equa t1 t2) var expr = Equa (sust expr (t1=:var)) (sust expr (t2=:var))


infer :: Float -> Sust -> Term -> Term -> Equation
infer n s var expr = leibniz (instantiate (prop n) s) var (expr)

step :: Term -> Float -> Sust -> Term -> Term -> Term
step term1 n sus var expr = compareEquation term1 (infer n sus var expr)

compareEquation :: Term -> Equation -> Term
compareEquation term1 (Equa t1 t2) 
	| term1==t1 = t2
	| term1==t2 = t1
	| otherwise = error "*** No se puede seguir instanciando ***"

statement :: Float -> Dummy -> Sust -> Dummy -> Dummy -> Term -> Term -> Term -> IO Term
statement n with sus using lambda var expr term1  = 
	do{ 
	putStrLn ("=== statement "++ show n++ " with " ++ show sus ++ " using lambda "++show var++"."++show expr);
	putStrLn (show (step term1 n sus var expr) );
	return (step term1 n sus var expr)
	}

statement' :: Float -> Term -> IO Term
statement' n (Var x) = do{ putStrLn ("Holis "++show x );return (Var x)}

proof :: Equation -> IO Term
proof (Equa t1 t2) = do { putStrLn ("Prooving "++ show (Equa t1 t2) ); putStrLn (show t1); return t1}

done :: Equation -> Term -> IO ()
done (Equa t1 t2) term1 = do { if term1==t2 then putStrLn "Proof succesful." else putStrLn "Proof unsuccesful."}



{-sust :: Term -> Sust -> Term (Listo)
instantiate :: Equation -> Sust -> Equation (Listo)
leibniz :: (Listo)
infer :: Int -> Equation -> Sust -> z -> E (Casi listo)
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



