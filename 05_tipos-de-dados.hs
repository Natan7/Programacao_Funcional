--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.

------------------------------------------------------------------
---- Not Run - data Triplo a b c = Triple a b c deriving (Eq,Show)
--- "Triplo" is constructor ----------

data Triple a b c = Triplo a b c deriving (Eq,Show)

tripleFst (Triplo a b c) = a
tripleSnd (Triplo a b c) = b
tripleThr (Triplo a b c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruplo a a b b deriving (Eq,Show)

firstTwo (Quadruplo a b c d) = (a,b)
secondTwo (Quadruplo a b c d) = (c,d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tupla1 a | Tupla2 a b | Tupla3 a b c | Tupla4 a b c d deriving (Eq,Show)

tuple1 (Tupla1 a) = Just a
tuple1 (Tupla2 a b) = Just a
tuple1 (Tupla3 a b c) = Just a 
tuple1 (Tupla4 a b c d) = Just a 

tuple2 (Tupla2 a b) = Just b
tuple2 (Tupla3 a b c) = Just b 
tuple2 (Tupla4 a b c d) = Just b 
tuple2 _ = Nothing

tuple3 (Tupla3 a b c) = Just c
tuple3 (Tupla4 a b c d) = Just c
tuple3 _ = Nothing

tuple4 (Tupla4 a b c d) = Just d
tuple4 _ = Nothing

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = Nil
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST = undefined

--insere uma nova chave na BST retornando a BST modificada
insert x NIL = (Node x NIL NIL)
insert x (Node a left right) | x<a = Node a (insert x left) right
                             | otherwise = Node a left (insert x right)

--retorna o Node da BST contendo o dado procurado ou entao NIL
search x NIL = error "Not Found!"
search x (Node a left right) | x==a = (Node a left right)
                             | x<a = (search x left)
                             | otherwise = (search x right)

--retorna o elmento maximo da BST
maximo x NIL = error "Not Exist!"
maximo x (Node a left right) | x<a = (maximo a right)
                              | otherwise = (Node a left right)

--retorna o elemento minimo da BST
--minimum = undefined
minimo x NIL = error "Not Exist!"
minimo x (Node a left right) | x>a = (minimo a left)
                              | otherwise = (Node a left right)

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
--predecessor = undefined
--predecessor x NIL NIL = error "Not Found!"
--predecessor x (Node a left right) | x==a = (Node a left right)
--                             | x<a = (search x left)
--                             | otherwise = (search x right)

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor = undefined

--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder = undefined
order = undefined
postOrder = undefined