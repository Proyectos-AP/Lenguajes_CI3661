-- importe lo necesario aquí ----

prop :: Float -> Equation 
prop num
  | num == 3.1  = (p <==> q) <==> r === p <==> (q <==> r)
  | num == 3.2  = (p <==> q) === (q <==> p)
  | num == 3.3  = p <==> p === true
  | num == 3.4  = p === p <==> true
  | otherwise = error "The statement doesn't exists"