{-
- Encontra o ultimo elemento de uma lista. Caso a lista seja vazia retorne o seguinte comando: error "Lista vazia!" 
-}
meuLast [] = error "Empyt List"
meuLast (x:[]) = x
meuLast (x:xs) = meuLast xs 

{-
- Encontra o penultimo elemento de uma lista. Caso a lista seja vazia ou tenha apenas um elemento retorne o seguinte comando: error "Lista sem penultimo" 
-}
penultimo [] = error "Empyt List"
penultimo (x:[]) = error "Not Exist in List"
penultimo (x:[xs]) = x
penultimo (x:xs) = penultimo xs


{-
- Retorna o k-esimo (k varia de 1 ate N) elemento de uma lista. Ex: elementAt 2 [4,7,1,9] = 7
-}
elementAt 1 [] = error "Index Out of Bounds"
elementAt _ [] = error "Empyt List"
elementAt 1 (x:xs) = x
elementAt i (x:xs) = elementAt (i-1) (xs)

{-
- Retorna o tamanho de uma lista. 
-} 
meuLength [] = error "Empyt List"
meuLength (x:[]) = 1
meuLength (x:xs) = 1 + meuLength(xs)

{-
- Retorna o inverso de uma lista. 
-}

meuReverso [] = error "Empyt List"
meuReverso (x:[]) = [x]
meuReverso (x:xs) = meuReverso(xs)++[x]

{-
- Diz se uma lista é palindrome. 
-}
isPalindrome [] = error "Empyt List"
isPalindrome xs = xs==meuReverso(xs)

{-
- Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
- Voce pode usar a funcao elem de Haskell
-}
compress xs = undefined

{-
- Varre a lista da esquerda para a direita e junta os elementos iguais. Ex: compact [2,5,8,2,1,8] = [2,2,5,8,8,1]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
compact xs = undefined
--filter p xs

{-
- Retorna uma lista de pares com os elementos e suas quantidades. Ex: encode [2,2,2,3,4,2,5,2,4,5] = [(2,5),(3,1),(4,2),(5,2)]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
encode xs = undefined

{-
- Divide uma lista em duas sublistas onde o ponto de divisao é dado. Ex: split [3,6,1,9,4] 3 = [[3,6,1],[9,4]]
-}
--split xs i = undefined
--split (_:[]) 1 = []
--split (x:xs) 1 = (x:[])--[xs]
--split (x:xs) i = x:(split xs (i-1))

--split [] _ = []
split (x:xs) 1 = x:[]
split (_:xs) 0 = xs
split (x:xs) i = ys++zs
    where
        ys = x:split xs (i-1)
        zs = split xs (i-1)
 
{-
- Extrai um pedaço (slice) de uma lista especificado por um intervalo. 
- Ex: slice [3,6,1,9,4] 2 4 = [6,1,9]
-}
slice (x:xs) 1 0 = []
slice (x:[]) 1 1 = x:[]
slice (x:xs) 1 imax = x:slice xs 1 (imax-1)
slice (_:xs) imin imax = slice xs (imin-1) (imax-1)


{-
- Insere um elemento em uma posicao especifica de uma lista. 
- Ex: insertAt 7 4 [3,6,1,9,4] = [3,6,1,7,9,4]
-}
insertAt _ _ [] = error "Empyt List"
insertAt el 1 xs = el:xs
insertAt el pos (x:xs) = x:insertAt el (pos-1) xs

{-
- Ordena uma lista em ordem crescente. Voce deve seguir a ideia do selectionsort onde os elementos 
- menores sao trazidos para o inicio da lista um a um. Esta funcao ja esta implementada.
-}
minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)
sort [] = []
sort xs = x:ys 
    where
        x = minList xs
        ys = sort (remove x xs) 

{-
- Calcula a soma de todos os elementos de uma lista usando foldr.
-}
mySum xs = foldr (+) (0) xs

{-
- Dada a funcao max que retorna o maximo entre dois numeros, escreva uma funcao que usa a função
- foldr e max para retornar o maximo de uma lista se a lista não é vazia.
-}
maxList [] = error "Empyt List"
maxList xs = foldr (max) (0) xs
{------------- Outra Forma----------------
maxList [] = error "Empyt List"
maxList (x:y:[]) = max x y
maxList (x:y:xs) = maxList( (max x y):xs )	
------------------------------------------}

{-
- Transforma uma string em uma palindrome acrescentando o reverso da string ao seu final sem usar a funcao reverse. 
- Ex: buildPalindrome [1,2,3] = [1,2,3,3,2,1]. 
-}
buildPalindrome xs = xs++(meuReverso xs)

{-
- Computa a media dos elementos de uma lista de numeros, sem usar nenhuma funcao pronta de listas.
-}
--mean xs = undefined
--mean (x:[]) = [ (y,z) | y<-1, z<-x]
--mean (x:xs) = [ (y,z)| y <- 1 , z <- x + mean (x:xs) , y>0||z>0]
--mean [] = error "Empyt List"
mean (x:xs) = mean' (x:xs) 2
mean' (x:[]) cont = x
mean' (x:xs) cont = ( x + mean' (xs) (cont+1) ) / cont
--mean' (x:xs) cont | (xs==[]) = x
--                  | otherwise = div ( x + mean' (xs) (cont+1) ) (cont)

{-
- Escreva a funcao myAppend que faz o append de uma lista xs com a lista ys, usando a função foldr. 
-}
myAppend xs ys = foldr (:) xs ys