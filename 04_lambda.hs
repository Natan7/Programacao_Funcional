--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes

pow = \x y -> if(y==0) then 1
              else if(y==1) then x
              else x * pow x (y-1)

fatorial = \x -> if(x==0) then 1
              else if(x==1) then 1
              else x * fatorial (x-1)


isPrime = \x -> notElem 0 ( map (mod x) ( take (x-2) (iterate (1+) 2) ) )


fib = \x -> if(x==1) then 1
              else if(x==2) then 1
              else fib (x-1) + fib (x-2)

mdc = \x y -> if(y==0) then x
              else mdc y (mod x y)

mmc = \x y -> div (x*y) (mdc x y)

coprimo = \x y -> if( (mdc x y) == 1 ) then True
                else False

goldbach x = undefined

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes

------------------------------------------------------
------firstall (a : []) = (\a -> a) a : []
------firstall (a : xs) = ((\a -> a) a : firstall(xs))
------------------------------------------------------

firstall = \(x:xs) -> tail xs

--meuLast = \(x:xs) -> meuLast(xs)
--meuLast = \(x:xs) -> if(x==[] && xs==[]) then error "Empyt List"
--                   else if(xs==[]) then x
--                   else meuLast xs

penultimo xs = undefined

elementAt = \i (x:xs) -> if(i==1) then x
                         else if(i==0) then error "Not Exist"
                         else elementAt (i-1) xs

meuLength xs = undefined
---meuLength = \(x:xs) -> if(i==1) then x
--                       else if(i==0) then error "Not Exist"
--                       else elementAt (i-1) xs

--meuReverso xs = undefined
--meuReverso = \(x:xs) -> x==[]

--isPalindrome = \xs -> xs==meuReverso(xs)

compress xs = undefined
compact xs = undefined
encode xs = undefined

split = \xs i -> [take i xs] ++ [drop i xs]

slice = \(x:xs) imin imax -> if(imin==1 && imax==0) then []
                             else if(imin==1) then x:slice xs 1 (imax-1)
                             else slice xs (imin-1) (imax-1)

--insertAt el pos xs = undefined
insertAt = \el pos (x:xs) -> if(pos==1) then el:x:xs
                             else x:insertAt el (pos-1) xs

sort xs = undefined

mySum = \(x:xs) -> foldr (+) (0) (x:xs)

maxList = \(x:xs) -> foldr (max) (0) (x:xs)

--buildPalindrome = \xs -> xs++(meuReverso xs)

--mean = \xs -> ( fromIntegral(sum xs ) )/( fromIntegral(meuLength xs) )      

myAppend = \(x:xs) (y:ys) -> foldr (:) (x:xs) (y:ys)

----------------------------
--------- Notes ------------
----------------------------
t1 = \x -> \_ -> x
t2 = \x _ -> x
----------------------------