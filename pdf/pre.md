---
title: Fantastic Morphisms and Where to Find Them
author: Zhixuan Yang and Nicolas Wu
patat:
  breadcrumbs: false
  images:
    backend: iterm2
  wrap: true
  margins:
    left:  10
    right: 10
  theme:
    emph: [italic, vividGreen]
    strong: [bold, dullBlue]
    header: [underline, bold, onRgb#252525]
    borders: [dullBlack, onDullWhite]
    blockQuote: [bold]
    # codeBlock: [onRgb#3a3a3a]
    code: [onRgb#3a3a3a]
    syntaxHighlighting:
      other:    [rgb#f08000]
      # normal: [bold, rgb#7070ff]	
      dataType: [bold, rgb#7070ff]	 
      operator: [bold, rgb#ffd58b]	 
      keyword:  [bold, rgb#ffd58b]
...

![](images/title.jpeg)


# The zoo of morphisms

Nick: **let's write a bestiary for the zoo of recursion schemes for
programmers!**


```
    mutumorphism
       __                            __                 
   .--()°'.'                        / _)  dyna morphism
  '|, . ,' __              _/\/\/\_/ /
   !_- .--()°'.'         _|         /
      '|, . ,'         _|  (  | (  |
       !_-(_\         /__.-'|_|--|_|
             
                                                     
   zygomorphism                                                                 
           (__)                 (=-'         
   `\------(oo)            /\/\  ))   anamorphism    
     ||    (__)          ~/    \/ |       
     ||w--||     \|/     | )___(  |       
 \|/                     |/     \||            _                              
                         |'      |'       |\__/,|   (`\                       
                                          |_ _  |.--.) )  cat amorphism
                                          (_T___) __  /                      
                                           (/ ( (/ (_/
```
(ASCII art by Morfina, PapaJ, ejm98)

# This talk 

Let's build a text editor "_vimacs_" in Haskell to demonstrate how recursion
schemes appear in practice.

. . .

The editor looks like this:
```
┌---------------------------------------------┐
|                                             |
|   __   __(_) _ __ ___    __ _   ___  ___    |  
|   \ \ / /| || '_ ` _ \  / _` | / __|/ __|   |
|    \ V / | || | | | | || (_| || (__ \__ \   |
|     \_/  |_||_| |_| |_| \__,_| \___||___/   |
|                                             |
|           MPC Special Version               |
|                                             |
|                                             |
|                                             |
|                                             |
|                                             |
|                                             |
┌─────────────────────────────────────────────┐
│File: README.md | Number of lines: 7         │
└─────────────────────────────────────────────┘
```

# Roadmap

* Saving to file

* Loading from file

* Insertion

* Deletion

* Sorting

* Spell checking


# Data structure

We will implement the internal data structure using _binary trees_.

```
"MPC22" is represented as

           P
         /  \
        /    \
       M      2
             / \
            /   \
           C     2
```

```haskell
data Seq a = Empty
           | Node  (Seq a)       -- left subtree
                   Int           -- size of the tree
                   a             -- payload data
                   (Seq a)       -- right subtree
```

# Initial Algebras

Such inductive datatypes are _initial algebras_ of functors.

In Haskell, initial algebras of functor `f` is denoted by 
```haskell
data Mu f = In { inOp :: f (Mu f) }
```

Defining a functor:
```haskell
data SeqF a x = Empty | Node    x    Int a    x
```
then `Mu (SeqF a)` is isomorphic to the old `Seq a`:
```haskell
data Seq a    = Empty | Node (Seq a) Int a (Seq a)
```


# Saving file

We need to convert the buffer into a list to save it:
```haskell
data SeqF a x = Empty | Node x Int a x

-- ...

toList :: Seq a -> [a]
toList (In Empty) = []
toList (In (Node lt _ el rt)) = toList lt ++ (el : toList rt)
```

**The program structure follows the data structure.**

# Catamorphisms

This pattern is captured by _catamorphisms_:
```haskell
toList' :: Seq a -> [a]
toList' = cata alg


cata :: Functor f => (f a -> a) -> Mu f -> a
cata alg (In t) = alg (fmap (cata alg) t)


alg :: SeqF a [a] -> [a]
alg Empty = []
alg (Node lf _ el rf) = lf ++ (el : rf)
```

# Loading file

When loading a file, we need to convert a list into a tree:
```haskell
fromList :: [a] -> Seq a
fromList [] = In Empty
fromList xs = In (Node (fromList l) n m (fromList r)) where
  n = length xs
  (l, m : r) = splitAt (n `div` 2) xs
```

. . . 

It is an instance of _structural co-recursion_, ie an _anamorphism_:

```haskell
ana :: Functor f => (c -> f c) -> c -> Nu f

fromList = nuToMu . ana coalg where
  coalg [] = Empty
  coalg xs = Node l n m r where
    n = ...
    (l, m:r) = ...
```

# Insertion & deletion 

Insertion and deletion can be decomposed into splitting and concatenating:
```haskell
split  :: Seq a -> Int -> (Seq a, Seq a)
concat :: Seq a -> Seq a -> Seq a


insert :: Int -> a -> Seq a -> Seq a
insert n x t = let (lt, rt) = split t n
               in (lt `concat` singletonSeq x) `concat` rt



delete :: Int -> Int -> Seq a -> Seq a
delete (l, r) t = let (lt, rt) = split t l 
                      (_, rrt) = split rt (r - l + 1)
                  in concat lt rrt
```


# Splitting 

Splitting a sequence into two at a given position _looks
like_ a _cata_ too:
```haskell
split :: Seq a -> Int -> (Seq a, Seq a)
split (In Empty) n = (In Empty, In Empty)
split (In (Node lt sz el rt)) n =
  if n <= size lt 
     then let (llt, rlt) = split lt n  
          in (llt, node rlt el rt)
     else let (lrt, rrt) = split rt (n - size lt - 1)
          in (node lt el lrt, rrt)
```

. . .

except some `lt` and `rt` making it not:

```haskell
split = cata alg where
  alg :: SeqF (Int -> (Seq a, Seq a)) -> Int -> (Seq a, Seq a)
  alg (Node split_lt sz el split_rt) = 
    if n <= size lt 
       then let (llt, rlt) = split_lt n  
            in (llt, node rlt el ???)
       else let (lrt, rrt) = split_rt (n - size ??? - 1)
            in (node ??? el lrt, rrt)
```
. . .

# Paramorphisms

A more expressive scheme is _paramorphisms_, which feed the original 
input `Mu f` to the algebra:
```haskell
para :: Functor f => (f (Mu f, a) -> a) -> Mu f -> a
para alg = alg . fmap (id /\ para alg) . inOp
```

```haskell
split' :: Seq a -> Int -> (Seq a, Seq a)
split' = para alg where
  alg :: SeqF a (Seq a, Int -> (Seq a, Seq a)) -> (Int -> (Seq a, Seq a))
  alg Empty n = (empty, empty)
  alg (Node (lt, split_lt) sz el (rt, split_rt)) n =
    if n <= size lt
     then let (llt, rlt) = split_lt n  
          in (llt, node rlt el rt)
     else let (lrt, rrt) = split_rt (n - size lt - 1)
          in (node lt el lrt, rrt)
```


# Concatenating
Sequence concatenating is a nested paramorphism:
```haskell
concat :: Seq a -> Seq a -> Seq a
concat (In Empty) rt = rt
concat lt (In Empty) = lt
concat lt@(In (Node llt lsz lel rlt)) rt@(In (Node lrt rsz rel rrt)) = 
  if lsz < rsz then node (concat lt lrt) rel rrt
               else node llt lel (concat rlt rt)
```
. . .

```haskell
concat' :: forall a. Seq a -> Seq a -> Seq a
concat' = para alg where
  alg :: SeqF a (Seq a, Seq a -> Seq a) -> Seq a -> Seq a
  alg Empty = \rt -> rt
  alg (Node (llt, _) lsz lel (rlt, concat_rlt)) = para alg' where
    alg' :: SeqF a (Seq a, Seq a) -> Seq a
    alg' Empty = node llt lel rlt
    alg' (Node (lrt, concat_lt_lrt) rsz rel (rrt, _)) = 
      if lsz < rsz then node concat_lt_lrt rel rrt
                   else node llt lel (concat_rlt (node lrt rel rrt))
```

# Bonus: sorting

To sort the lines, the core is
```haskell
sortLines :: [Seq Char] -> Seq Char
sortLines = foldr (\l r -> node l '\n' r) empty . quickSort
```
Can we eliminate building the intermediate list?

. . .

```haskell
sortLines' :: [Seq Char] -> Seq Char
sortLines' [] = empty
sortLines' (a:as) = node (sortLines' l) 
                         '\n' 
                         (node m '\n' sortLines' r) where
  l = [ b | b <- as , b < a  ]
  r = [ b | b <- as , b >= a ]
```

# Hylomorphisms

This generating-consuming pattern is called _hylomorphisms_:

```haskell
hylo :: Functor f => (f a -> a) -> (c -> f c) -> c -> a
hylo a c = a . fmap (hylo a c) . c


sortLines' :: [Seq Char] -> Seq Char
sortLines' = hylo combine partition where
  partition :: [Seq Char] -> SeqF (Seq Char) [Seq Char]
  partition [] = Empty
  partition (a : as) = Node [ b | b <- as , b < a ] 0 a [ b | b <- as , b >= a ]

  combine :: SeqF (Seq Char) (Seq Char) -> Seq Char
  combine Empty = empty
  combine (Node l _ m r) = node l '\n' (node m '\n' r)
```

# Spell checking

A word might be a typo if its _edit distance_ to a dictionary word is > 0 and < 3.

Dynamic programming is also captured by a recursion scheme:

```haskell
dyna :: Functor f => (f (Cofree f a) -> a) -> (c -> f c) -> (c -> a)
dyna = ...

...

editDistance :: String -> String -> Nat
editDistance xs ys = dyna alg (schedule n) (m, n) 
  where ...
```

# The zoo of morphisms

![](images/table.png)

# The point of recursion schemes

**Recursion schemes structure not only our code but also our reasoning**.

```

```

A small example, we reason:
```haskell
  toList . fromList
= cata alg . nuToMu . ana coalg
= {- fusion laws -}
  hylo alg coalg
= {- id = alg . id . coalg -}
  id
```

# A Brief categorical picture

Almost all recursion schemes are hylomorphisms with coalgebras obtained from adjunctions [Hinze, Wu, and Gibbons 2015]:

**Given an adjunction L ⊣ R and a conjugate pair (σ,τ), a recursive coalgebra c gives
rise to a new one (σ ⋅ L c).**

. . .

1. `U ⊣ Cofree` leads to dynamorphisml
2. `Δ ⊣ ×` leads to mutumorphism (mutual recursion)
3. `_× P ⊣ P => _` leads to accumulation 
4. adjunctions are composable, so ...
