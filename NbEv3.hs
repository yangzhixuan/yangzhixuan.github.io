{-

% An Algorithmic Reconstruction of Normalisation by Evaluation
% Zhixuan Yang, the University of Exeter
% Apr 24, 2026


In the past a few years, I spent quite a lot of time on learning the categorical glueing
technique for proving properties of programming languages, in particular,
the normalisation properties of type theories and programming languages (if you
are interested in learning this as well, a good starting point is the paper
[Semantic Analysis of Normalisation by Evaluation for Typed Lambda
Calculus](https://arxiv.org/abs/2207.08777) by Marcelo Fiore, but you need to know
some basic category theory before reading this though).  Those proofs can
usually be done in a constructive meta-theory, which means that it is in
principle possible to extract the algorithmic content of the proof, giving us a
_normaliser_ that takes in any program as input and outputs the normal form of
the input. The normaliser works by first evaluating the input program as a value
in certain semantic domain, which can then be 'reified' into a normal form.  This
way of normalising programs is called _normalisation by evaluation_ (NbE).

Normalisation by evaluation is known to be more efficient in practice than
substitution-based normalisers. Glueing proofs nicely explains _why normalisation
by evaluation is correct_, but I find that they don't tell us much about _why
normalisation by evaluation is *fast*_. In this article, I will try to shed
some light on the latter question by showing how we can spot the sources of
inefficiency in a naive substitution-based normaliser, optimise these inefficiency
problems with some standard algorithmic tricks, and eventually obtain NbE. In fact,
through this process we will also see some inefficiency problems in NbE, and in
the end we will end up with a normaliser that is _asymptotically better_
than a standard NbE normaliser on some input (and never worse on all input).
-}


{-
Preliminary setup
--------------------------------------------------------------------------------

In this article, we will use Haskell as our meta-language. Haskell is a lazy
language. Although laziness never harms the time complexity of programs (and
can sometimes make asymptotical improvement!), thinking about the
running cost of lazy programs is a nightmare compared to strict programs.
Therefore we will turn on the `Strict` extension of GHC, which makes laziness an
_opt-in_ feature (by decorating binding variables or data fields with `~`) rather
than the default behaviour. Enabling `Strict` also makes porting the code in
this article to other strict programming languages (say, C or OCaml) a lot
easier, as there is no implicit laziness that our algorithms secretly rely on.
-}

{-# LANGUAGE Strict #-}
module NbE where

{-
Note however, `Strict` doesn't change the behaviour of existing definitions from
other modules, such as those from `Prelude`, so the built-in list type `[a]`
remains to be lazy lists of lazy payloads.
-}

type LazyList a = [a]

{-
If we want strict lists, we can define our own `data List a = Nil | Cons a (List
a)` in this module (or any other module with `Strict` enabled).
-}

{-
Since our topic is about the _efficiency_ rather than the _correctness_ of NbE, we
will simply use untyped lambda calculus as our object language in this article.
Consequently, our normalisers will not terminate on input programs that do not
have a normal form -- we don't care. We are only interested in programs that
do have normal forms.

We will use de Bruijn indices to represent variables formally (but we will still
use named variable in our informal discussion). The following code is some basic
definitions for the syntax of lambda calculus using de Bruijn indices.  In this
article I will assume that you are already familiar with them -- if not, try
search 'tutorial of lambda calculus' or 'de Bruijn indices' online and you can
find a lot of resources, such as
[this](https://www.irif.fr/~mellies/mpri/mpri-ens/biblio/Selinger-Lambda-Calculus-Notes.pdf)
and [this if you prefer code over maths](https://plfa.github.io/Lambda/).
Otherwise you can skim over the definitions and go to [the next
section](#naive-normaliser); nothing unusual here.
-}

data Tm = Var Int | App Tm Tm | Abs Tm deriving (Show, Eq)

infixl `App`

-- `subst s x t` substitutes `t` for every occurrence of `x` in `s`.
subst :: Tm -> Int -> Tm -> Tm
subst (Var n)   x t = if x == n then t else Var n
subst (App f a) x t = App (subst f x t) (subst a x t)
subst (Abs b)   x t = Abs (subst b (x + 1) (shift 0 1 t))

-- `shift x i t` increments all variables in `t` that are `>= x` by `i`.
shift :: Int -> Int -> Tm -> Tm
shift x 0 t = t
shift x i (Var n) = if n >= x then Var (n + i) else Var n
shift x i (App f a) = App (shift x i f) (shift x i a)
shift x i (Abs b) = Abs (shift (x+1) i b)

-- `beta b a` stands for beta-reduction for `(\. b) a` (substituting `a` for
-- variable of de Bruijn index 0 in `b`). Because `b` lives in one level deeper
-- than `a`, we use `shift 0 1` to bring `a` to the same level of `b`.
-- Also after substitution, we want to remove the lambda abstraction, so we
-- need to do `shift 0 (-1)` for the result of substitution.
beta :: Tm -> Tm -> Tm
beta b a = shift 0 (-1) (subst b 0 (shift 0 1 a))

-- `fv t` computes the biggest de Bruijn index of all free variables in a term
-- `t`. If a term has no free variables, the result is -1.
fv :: Tm -> Int
fv (Var n) = n
fv (Abs b) = max (-1) (fv b - 1)
fv (App f a) = max (fv f) (fv a)

-- `showTm ns t` computes the string representation of a term and the precedence
-- of the outermost syntactic constructor. The argument `ns` is the list of
-- names for the free variables.
showTm :: Int -> Tm -> (String, Int)
showTm ns (Var n) = ("x" ++ show (ns - n - 1), 100)
showTm ns (Abs b) =
  let (s, _) = showTm (ns + 1) b
  in ("\\x" ++ show ns ++ ". " ++ s, 0)
showTm ns (App f a) =
  let (s1, p1) = showTm ns f
      (s2, p2) = showTm ns a
      s1' = if p1 >= 50 then s1 else "(" ++ s1 ++ ")"
      s2' = if p2 > 50  then s2 else "(" ++ s2 ++ ")"
  in (s1' ++ " " ++ s2', 50)

-- instance Show Tm where
--   show t = let n = fv t in fst (showTm (n+1) t)

-- >>> Abs (Var 0 `App` Var 0)
-- \x0. x0 x0

-- >>> Abs (Var 0 `App` Var 0) `App` (Abs (Var 0))
-- (\x0. x0 x0) (\x0. x0)

-- >>> Abs (Var 0 `App` Var 0) `App` (Abs (Var 0)) `App` (Abs (Var 0))
-- (\x0. x0 x0) (\x0. x0) (\x0. x0)

-- >>> Abs (Var 0 `App` Var 0) `App` (Abs (Abs (Var 1)) `App` Abs (Var 0))
-- (\x0. x0 x0) ((\x0. \x1. x0) (\x0. x0))


{-
<a id="naive-normaliser"></a>

Applicative-order normalisation
--------------------------------------------------------------------------------

Now let's start with a simple-minded normaliser. We would like to define a function
-}

nf0 :: Tm -> Tm

{-
that takes in an arbitrary lambda term, which is allowed to have free variables,
and should produce a normal form beta-equivalent to the input. Recall that the
definition of a term `t` being in normal form is given inductively as follows:

  1. `t` is a variable applied to zero or more arguments, where each argument is
     in normal form, i.e. `t ≡ v a_1 a_2 ... a_n` where `v` is a variable, `n ≥
     0`, and each `a_i` is in normal form (the base case of a normal form is
     when `n = 0`).

  2. `t` is a lambda abstraction applied to a normal form, i.e. `t ≡ \x. b` where
     `b` is in normal form.

When the input to `nf0` is a variable, we are already done:
-}

nf0 (Var n) = Var n

{-
When the input is a lambda abstraction, we can just recursively compute the
normal form of the function body:
-}
nf0 (Abs b) = Abs (nf0 b)

{-
The interesting case is when the input is an application `f a`. A simple idea is
to first recursively compute the normal forms `f'` and `a'` of `f` and `a`
respectively, and then

  1. if `f'` is a lambda abstraction `\x. b`, `f' a'` is not a normal form because
     `(\x. b) a'` is beta-reducible, so we can do this beta-reduction and re-normalise
     the result;

  2. if `f'` is not a lambda abstraction, it is a variable applied to a (possibly empty)
     list of normal forms, sometimes called a _spine_. In this case, adding one more
     argument `a'` keeps it normal.

In total, we have

<a id="nf0-app"></a>
-}
nf0 (App f a) =
   let f' = nf0 f
       a' = nf0 a
   in case f' of
        Abs b -> nf0 (beta b a')
        _ -> App f' a'

{-
Let's do some basic sanity checks of our naive normaliser using Church numerals.
This article is written as a `.hs` file so that I can use
haskell-language-server to run small tests on the fly.  Unfortunately you can't
do this if you are reading the generated html file. If you want to play with the
code more interactively, you can download the source code
[here](https://yangzhixuan.github.io/NbE.hs) and load it into your editor.
-}

-- Church numeral increment
cinc :: Tm
cinc = Abs (Abs (Abs (Var 2 `App` Var 1 `App` (Var 1 `App` Var 0))))

-- Church numeral 0
c0, c1, c2 :: Tm
c0 = Abs (Abs (Var 0))
c1 = cinc `App` c0
c2 = cinc `App` c1

-- >>> nf0 c1
-- \x0. \x1. x0 x1

-- >>> nf0 c2
-- \x0. \x1. x0 (x0 x1)

-- Church numeral addition
cadd :: Tm
cadd = Abs $ Abs $ Abs $ Abs $
  Var 3 `App` Var 1 `App` (Var 2 `App` Var 1 `App` Var 0)

-- >>> nf0 (cadd `App` c2 `App` c2)
-- \x0. \x1. x0 (x0 (x0 (x0 x1)))

-- Church numeral multiplication
cmul :: Tm
cmul = Abs $ Abs $ Abs $ Abs $
  Var 3 `App` (Var 2 `App` Var 1) `App` Var 0

-- >>> nf0 (cmul `App` c2 `App` (cinc `App` c2))
-- \x0. \x1. x0 (x0 (x0 (x0 (x0 (x0 x1)))))

-- >>> nf1 ((cmul `App` c2 `App` (cinc `App` c2)) `App` Var 0 `App` Var 1)
-- x1 (x1 (x1 (x1 (x1 (x1 x0)))))

-- >>> nf1 (Var 0 `App` Var 1)
-- x1 x0

-- >>> nf0 ((cmul `App` c2 `App` (cinc `App` c2)) `App` Var 0 `App` Var 1)
-- x1 (x1 (x1 (x1 (x1 (x1 x0)))))

{-
Great, basic tests check out! The normaliser `nf0` always reduces the argument
of a function application to normal form before applying it to the function body.
For this reason, it is usually called _applicative-order normalisation_.
-}


{-
Inefficiency of applicative-order normalisation
--------------------------------------------------------------------------------

The normaliser `nf0` can be inefficient for a number of reasons.

First of all, in the case `nf0 (App f a)` [above](#nf0-app), `nf0` normalises
the argument `a` to `a'` before applying `a'` to the function, but it is totally
possible that `f` doesn't use its argument at all, so the effort spent on
computing `a'` is wasted. This problem isn't hard to fix in Haskell: we can make the
normalisation of the argument (i.e. the variable `a'` in `nf0`) lazy by
annotating `a'` with `~`, which is the syntax of Haskell to make let-bindings
lazy when `Strict` is enabled.

Secondly, we normalised the function `f` as well before substituting the
(normalised) argument `a'`, but we still need to re-normalise after substitution,
so we normalised the function body twice:

  1. when we don't have the argument, and

  2. when the argument is instantiated to `a'`.

This can be inefficient in some situations; for example, consider a term
of the form
```
(\x. x expensiveTm) (\y. M)  -- where y doesn't occur in M
```
where normalising the function body `x expensiveTm` is expensive. However, if we
know the argument `x` will be instantiated to the constant function `\y. M`, the
function body becomes `(\y. M) expensiveTm`, which would enable us to avoid
normalising `expensiveTm` as long as we manage to fix the inefficiency of
normalising unused arguments mentioned in the last paragraph.

The last point about why we don't want to fully normalise the function `f` in fact
is another reason why we don't want to normalise the argument `a`, because in the
function body of `f`, the argument `a` may be used as a function as well! For example,
consider the term
```
(\y. y (\z. M))  (\x. x ExpensiveTm)
```
In `\y. y (\z. M)`, the argument `y` is not unused, but we still don't want to full
normalise its concrete argument `\x. x ExpensiveTm` before substituting it for `y`,
because the argument `\x. x ExpensiveTm` will be used as a function after substitution.
-}

{-
Fixing the inefficiency, a plan
-----------------------------

From the discussion above, we see that the problem with `nf0` is that when it
sees a function application `f a`, it may too eagerly normalise `f` and `a`.
If we want to fix this source of inefficiency, what is the minimum amount of
normalisation work that we should do to `f` and `a` instead?

  1. We must at least normalise `f` to expose its outermost lambda abstraction,
  because that's what we need for doing a beta-reduction. We can normalise `f`
  more, but exposing its outermost layer is the minimum requirement.  In jargon,
  we must at least normalise `f` to its the _weak head normal form_ (WHNF).

  2. We have no constraints on how much normalisation that we do for `a`. We
  can leave it totally unnormalised when supplying it to the function, or we can
  also normalise it to expose the outermost lambda abstraction, or we can fully
  normalise it as we did in `nf0`.
-}

{-
For the function `f`, doing the minimum amount of work (i.e. normalising `f` to
its WHNF) is optimal. To see this, suppose the WHNF of `f` is `\x. M`. In the
term `M`, the variable `x` may be used both as a function and as an argument, so
`M` looks like
```
... (x M1) ... (M2 x) ...
```
By substituting (some partially normalised form of) the argument `a` for `x` before
normalising this function body `M`, we may be able to avoid normalising `M1` if
`a` doesn't use `M1`. And substituting `a` in before normalising `M` doesn't
create any additional overhead.
-}

{-
On the other hand, how much we normalise the argument `a` before supplying it to
the function is more interesting:

  1. As we said before, full normalisation is not optimal because in the
  function body `a` may be unused or it may be used as a function.

  2. Completely no normalisation is a bad idea too because the argument of `f`
  may occur multiple times in `f`. Substituting unnormalised `a` for each of
  these occurrences leads us to duplicated normalisation work for `a`.

Our third choice is to normalise the argument `a` to the same extent as `f`,
since the argument `a` may be used as a function inside `f` as well. A precise
way to summarise this choice is that we would like to compute _weak normal
forms_ (WNFs) of terms (in the process of full normalisation):
a term is in weak normal form if it is either

  1. a lambda abstraction, or

  2. a variable applied to zero or more arguments that are weak normal form
  themselves.

Moreover, we can (weakly) normalise `a` _lazily_, so that if the function body
of `f` doesn't actually use its argument, the work of (weakly) normalising `a`
can be saved.  Although we will see later that this strategy is not optimal
either, it is pretty decent one, so let's make it our plan for now.


-}

{-
Weak-applicative-order normalisation
--------------------------------------------------------------------------------
-}

{-
Following our plan, let's define a new function `wnf1 :: Tm -> Tm` for
normalising a term to WNF. The way this function works is a bit similar to
`nf0`, if we see an application, we reduce the function and the argument to WNF,
and if the WNF of the function is a lambda abstraction, we do a beta-reduction
and re-normalise to WNF. The difference from `nf0` is that when `wnf1` sees a
lambda abstraction, it doesn't do anything (whereas `nf0` would continue to
normalise under the binder).
-}
wnf1 :: Tm -> Tm
wnf1 (App f a) =
  let f' = wnf1 f
      ~a' = wnf1 a
  in case f' of
       Abs b -> wnf1 (betaL b a')
       _ -> App f' a'
wnf1 t = t
{-
Note that the variable `a'` is annotated with `~`, which makes `a'` lazily evaluated
(if `b` doesn't use its argument, `a' = wnf1 a` will not be computed).  We also
need to use a version of `beta` that is lazy in its second argument. Until
someone designs 'strictness polymorphism' and implements it in GHC, we have no
choice but to duplicate our `beta` and `subst` functions with a `~` annotation
in the arguments that we'd like to be lazy:
-}
betaL :: Tm -> Tm -> Tm
betaL b ~a = shift 0 (-1) (substL b 0 (shift 0 1 a))

substL :: Tm -> Int -> Tm -> Tm
substL (Var n)   x ~t = if x == n then t else Var n
substL (App f a) x ~t = App (substL f x t) (substL a x t)
substL (Abs b)   x ~t = Abs (substL b (x + 1) (shift 0 1 t))

{-
Just to check our laziness annotation is working as expected:
-}
-- >>> betaL (Abs (Var 0)) undefined
-- \x0. x0

-- >>> beta (Abs (Var 0)) undefined
-- Prelude.undefined

{-
Our new function for full normalisation then uses `wnf1` to compute weak normal
forms in the case of a function application:
-}
nf1 :: Tm -> Tm
nf1 (Var v) = Var v
nf1 (Abs b) = Abs (nf1 b)
nf1 (App f a) =
  let f' = wnf1 f
      ~a' = wnf1 a
  in case f' of
       Abs b -> nf1 (betaL b a')
       _ -> App (reify1 f') (reify1 a')

{-
When the weak normal form `f'` of the function is not a lambda abstraction,
we need to turn the weak normal forms `f'` and `a'` into (full) normal forms.
This is done by the following function that triggers normalisation under
lambda abstractions:
-}
reify1 :: Tm -> Tm
reify1 (Var v) = Var v
reify1 (Abs b) = Abs (reify1 (wnf1 b))
reify1 (App f a) = App (reify1 f) (reify1 a)

{-
One thing to notice is that an alternative definition of `nf1` is simply:
-}
nf1' :: Tm -> Tm
nf1' = reify1 . wnf1
{-
The explicit definition `nf1` above is the fused version of these two functions
composed together. The fused version `nf1` is slightly more efficient than
`nf1'` because it avoids building the intermediate weak normal form built by
`wnf1` and consumed by `reify1`. Deriving the fused normalising function from
`wnf1` and `reify1` is usually easy, so in the rest of this article we will
focus on optimising `wnf1` and `reify1` and define normalisation as their
composition.
-}

{-
To see the improvement of our new normaliser `nf1` over `nf0`, let's construct a
term `(\x. x expensiveTm) (\x. c0)` and let `expensiveTm` be the conjunction of
a large number of Church Boolean values of truth.
-}

-- A term that is expensive to normalise
expensiveTm :: Int -> Tm
expensiveTm n = Abs (churchNum n `App` (cAnd `App` cTrue) `App` cTrue)

-- Church encoding of Boolean values
cTrue, cFalse :: Tm
cTrue = Abs (Abs (Var 1))
cFalse = Abs (Abs (Var 0))

-- Conjunction of Boolean values
cAnd :: Tm
cAnd = Abs (Abs (Abs (Abs (Var 3 `App` (Var 2 `App` Var 1 `App` Var 0) `App` Var 0))))

-- `churchNum n` is `cinc` applied to zero `n` times.
churchNum :: Int -> Tm
churchNum n = foldr (\_ r -> cinc `App` r) c0 [1 .. n]

-- This takes a lot of time to compute
-- >>> nf0 $ (Abs (Var 0 `App` expensiveTm 10000)) `App` Abs c0

-- This takes less than a second to compute on my machine
-- >>> nf1 $ (Abs (Var 0 `App` expensiveTm 10000)) `App` Abs c0
-- \x0. \x1. x1


{-
Great, our optimisation worked (at least for this particular term)! We may call
the normaliser `nf1` _weak-applicative-order normalisation with meta-level
laziness_. Meta-level laziness refers to the fact that function arguments are
reduced lazily through the laziness of our meta-language, Haskell.
-}


{-
The lazy tagging trick
--------------------------------------------------------------------------------
-}

{-
The function `wnf1` above uses the function `beta` to substitute the actual
argument `a'` for the binder of the lambda abstraction. Recall that the
function `beta` is defined by
```haskell
beta b a = shift 0 (-1) (subst b 0 (shift 0 1 a))
```
So `beta b a` is not a cheap operation. We need to first use `shift` to
bring the argument `a` to the next de Bruijn level, then traverses the
function body `b` to do the actual substitution, and then use `shift` again
to bring the result back to the previous de Bruijn level.

Can we do substitution more efficiently? Last year I mentioned on Mastodon
that I had a data structure for implementing (capture-avoiding) substitution
that avoids the cost of traversing the term. I plan to talk about this data
structure more at this year's [Logic Colloquium](https://logiccolloquium2026.github.io),
but here because of some special property of how `wnf1` works, we actually
don't need any fancy data structure. A very common algorithmic trick suffices.
-}

{-
The trick that we are going to use to make substitution efficient is _lazy
tagging_. The trick is best explained by a simple example. Suppose we have a
binary tree storing integers
-}
data Tree = Leaf Int | Node Tree Tree
{-
and suppose that we would like to perform the operation of adding some integer
to every integer of a tree:
-}
add :: Int -> Tree -> Tree
add n (Leaf i) = Leaf (n + i)
add n (Node l r) = Node (add n l) (add n r)
{-
The operation `add n t` works in time `O(size t)`. The trick of lazy tagging
optimises `add` to `O(1)` by changing the tree datatype to
-}
data Tree' = Leaf' Int | Node'  Int Tree' Tree'
{-
where the constructor `Node` now has an additional `Int` field (the lazy tag)
storing a number that is logically added to all the leaves of the tree. With
this additional field, `add` can be implemented as
-}
add' :: Int -> Tree' -> Tree'
add' n (Leaf' i) = Leaf' (n + i)
add' n (Node' m l r) = Node' (n + m) l r
{-
On the other hand, whenever we want to access the two sub-trees of a node, we
should always push down the lazy tag first:
-}
push :: Tree' -> Tree'
push (Node' m l r)
  | m /= 0 = Node' 0 (add' m l) (add' m r)
push t = t
{-
This `push` operation is also `O(1)` because `add'` is `O(1)`, so having an
additional `push` whenever we want to pattern match a `Node'` doesn't change the
time complexity. For example, if we want to sum the integers of a tree, we do
-}
sumTree :: Tree' -> Int
sumTree (Leaf' i) = i
sumTree t = case push t of Node' _ l r -> sumTree l + sumTree r
{-
In Haskell, we can even use view patterns and pattern synonyms to make the call
of `push` completely transparent.

The reader might be wondering how lazy tagging is different from laziness of
Haskell? Why don't we simply make our trees lazy by writing
-}
data Tree'' = Leaf'' Int | Node'' ~Tree ~Tree
{-
The key difference is because of this line:
```haskell
add' n (Node' m l r) = Node' (n + m) l r
```
which combines two lazy tags `n` and `m` into one tag `n + m`. In comparison,
lazy evaluation of Haskell isn't clever enough to do this; after applying
`add` for `Tree''` a few times, we would have nested thunks
`add n_1 (add n_2 (...  (add n_m t)))`, for which we need `O(m)` rather
than `O(1)` time to push these added integers one-level down.
-}

{-
Weak-applicative-order normalisation with lazy substitutions
--------------------------------------------------------------------------------

Coming back to `wnf1`, we'd like to use lazy tagging to make substitution more
efficient. Just like the `add` example above, when we want to substitute an argument
for the binder variable in a function body, we will just lazily _remember_ this
substitution at the root of the syntax tree without actually performing the substitution.

Moreover, recall that our `wnf1` only normalises a function body when we have a
concrete value for the function argument. Consequently, if there are two
variables `x` and `y` in a term such that `y` is bound in the scope of `x`, like
in
```
(\x. ... (\y. ... x ...) N ...) M
```
the variable `x` always receives its concrete value (`wnf1 M` for the term above),
before `y` receives its concrete value (`wnf1 N` for the term above). Therefore
we can simply represent a lazy substitution applied to a term as a list of terms.
More precisely, we will lazily store a substitution
```
((((t [a1/x1]) [a2/x2]) [a3/x3]) ... [aN/xN])
```
as pair of the original term `t` and the list `[aN, ..., a3, a2, a1]`. Applying a
new substitution (for the de Bruijn index 0) is just cons-ing a value to the head
of the list.

Notice the correctness of this representation hinges on the fact that we are doing
*capture-avoiding substitution*. When we substitute a value `aN` for a bound *variable `xN`,
we can just cons `aN` to the list `[..., a3, a2, a1]` of existing substitutions for the
*outer bound variables* because we know for sure that `xN` doesn't occur in the arguments
`[..., a3, a2, a1]` from outer layers. For example, consider the term
```
(\x. ... (\y. ... x ...) N ...) M
```
We first substitute `wnf1 M` for `x` and then later substitute `wnf1 N` for
`y`, and we know for sure that `y` doesn't occur in `wnf1 M` because our
substitution is capture avoiding. Therefore the lazy substitution for the inner
function body can just be `[wnf1 N, wnf1 M]`, which means that variable of de Bruijn
index 0 (i.e. `y`) has been substituted by `wnf1 N`, the variable of de Bruijn
index 1 (i.e. `x`) has been substituted by `wnf1 M`.

*Side Remark*: In modal type theory, there is a concept called [_delayed
substitution_](https://arxiv.org/abs/1601.01586) for dealing with type formers
that do not commute with substitutions.  There is similarity between delayed
substitution and our idea here, but the motivation is completely different:
loc cit the problem is that some syntactic constructors don't commute with
substitution, whereas our syntactic constructors do commute with substitution and
we delay substitution for efficiency considerations.
-}


{-
Enough with talking, let's implement our ideas. The first thing that we are going
to do is to define the type of weak normal forms with lazy substitutions.
Previously, `wnf1` has type `Tm -> Tm`, but now we want to work with lazy
substitutions, so the output type can't be `Tm` anymore (the input term can still
be `Tm`). We need to define a new type for the output of `wnf1`. The name 'weak
normal forms with lazy substitutions' is too verbose, so we will just call them
_values_. A value is either

  1. a lambda function, represented by `Closure2` below, which two fields: the
  function body and the lazy substitution applied to this function (stored
  as a lazy list of values); or

  2. a variable applied to a list of arguments, represented by `Spine2` above,
  which also have two fields: the de Bruijn index of the variable and the arguments
  (stored also as a lazy list of values).
-}

data Val2 = Closure2 Tm Subst2 | Spine2 Int (LazyList Val2)
type Subst2 = LazyList Val2

{-
Here we use lazy lists (which is just the default Haskell list type `[a]`) rather
than strict lists because we want to make normalisation of function arguments
lazy, like how we did in `wnf1`. Strictly speaking we only need the payloads of
the list to be lazy and the structure of the list itself can remain strict:
```haskell
data PayloadLazyList a = Nil | Cons ~a (PayloadLazyList a)
```
but let's just use `LazyList` (lazy lists of lazy payloads) because I am too
lazy to re-define the standard list functions that we are going to use.
-}

{-
The type of `wnf1 :: Tm -> Tm` becomes then
```haskell
wnf2 :: Tm -> Subst2 -> Val2
```
where the output is a value (i.e. a weak normal form with lazy substitutions) and
the input is a term paired with lazy substitutions applied to this term. Whenever
`wnf2` sees a variable, the variable logically has been replaced by the lazy
substitution, so we just look up the substitution:
```haskell
wnf2 (Var n) sub = sub !! n
```
When `wnf2` sees a lambda abstraction, it is already a weak normal form, so we
just store it along with the lazy substitution:
```haskell
wnf2 (Abs b) sub = Closure2 b sub
```
The interesting case is again function application `App f a`. We will do recursion on `f`
and `a` as before:
```haskell
wnf2 (App f a) sub =
  let f' = wnf2 f sub
      ~a' = wnf2 a sub
  in case f' of
       Closure2 b sub' -> _what_do_we_do_here
       Spine2 v as -> Spine2 v (a' : as)
```
but what should we do this time when the function is a lambda abstraction? Previously we
did `wnf1 (beta b a')` where
```haskell
beta b a = shift 0 (-1) (subst b 0 (shift 0 1 a))
```
Substituting `a'` for the variable `0` in `b` should now be just `a' : sub'`.
However, we now need to shift the de Bruijn indices of the whole substitution by
1 rather than just `a`, because `sub'` is an lazy substitution applied `Abs b`
rather than `b`. Therefore our new substitution for `b` should be `shift0List 1
(a' : sub')` where
-}
shift0Val :: Int -> Val2 -> Val2
shift0Val i (Spine2 n as) = Spine2 (n + i) (shift0List i as)
shift0Val i (Closure2 b sub) = Closure2 b (shift0List i sub)

shift0List :: Int -> Subst2 -> Subst2
shift0List i = map (shift0Val i)

{-
Note that `shift0Val` doesn't need to shift the indices in the function body
`b`, because these variables logically have already been substituted by `sub`.
-}

{-
Another difficulty is the `shift 0 (-1)` in the definition of `beta b a`.
Because now our substitution is lazy, we don't really have the result of
substitution to perform this `shift 0 (-1)` on. However, in `wnf1` we really
wanted to do is `wnf1 (beta b a')`, and we can rewrite it a bit:
```
  wnf1 (beta b a')
= wnf1 (shift 0 (-1) (subst b 0 (shift 0 1 a')))
= shift 0 (-1) (wnf1 (subst b 0 (shift 0 1 a')))
```
The first equality is just the definition of `beta` and the second equality is
because removing the variable `0` that is substituted away before or after weak
normalisation gives the same result. This observation then allows us to
translate `wnf1` to `wnf2`:
-}
wnf2 :: Tm -> Subst2 -> Val2
wnf2 (Abs b) sub = Closure2 b sub
wnf2 (Var n) sub = sub !! n
wnf2 (App f a) sub =
  let f' = wnf2 f sub
      ~a' = wnf2 a sub
  in case f' of
       Closure2 b sub' -> shift0Val (-1) (wnf2 b (shift0List 1 (a' : sub')))
       Spine2 v as -> Spine2 v (a' : as)

{-
Reifying values to normal forms works the same as `reify1`, except that for the case
of a lambda abstraction `Closure2 t sub`, the new substitution for the function
body `b` should now be `Spine2 0 [] : shift0List 1 sub`, where `Spine2 0 []` means
that the bound variable `0` of the function body `b` is still `0` itself, applied to no
arguments. And `shift0List 1` is applied to `sub` because `sub` is a substitution
applied to `Abs b` rather than `b`.
-}

reify2 :: Val2 -> Tm
reify2 (Closure2 b sub) = Abs (reify2 (wnf2 b (Spine2 0 [] : shift0List 1 sub)))
reify2 (Spine2 v as) = foldr (\a r -> r `App` reify2 a) (Var v) as

{-
Putting things together, we have our new normaliser:
-}

nf2 :: Tm -> Tm
nf2 t = let n = fv t
        in reify2 (wnf2 t (reflect2 n))

reflect2 :: Int -> Subst2
reflect2 n = map (\v -> Spine2 v []) [0 .. n]

{-
The function `reflect2` creates the trivial substitution for the free variables
of the term `t`, substituting each free variable `v` for `v` itself. (Side
remark: `reflect2` is rather trivial here, but if we do normalisation for typed
languages with _eta-equalities_, this `reflect` function would be more
interesting.)
-}


{-
Let's do some sanity checks again:
-}
-- >>> nf2 c2
-- \x0. \x1. x0 (x0 x1)

-- >>> nf2 (cmul `App` c2 `App` (cinc `App` c2))
-- \x0. \x1. x0 (x0 (x0 (x0 (x0 (x0 x1)))))

-- >>> nf2 (cmul `App` c2 `App` (cinc `App` c2) `App` Var 0 `App` Var 1)
-- x1 (x1 (x1 (x1 (x1 (x1 x0)))))

-- >>> nf2 (cmul `App` (cinc `App` c2) `App` (cinc `App` c2))
-- \x0. \x1. x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 x1))))))))


{-
Defunctional normalisation by evaluation
--------------------------------------------------------------------------------

Now let's move on to optimise `wnf2` further. A notable source of inefficiency
is the work that we needs to do for shifting de Bruijn indices. De Bruijn
indices are only for representing variables in a scope-safe way. They shall not
play any central role in normalisation. Therefore we'd like to optimise them out.

Let's first work on the shifting in this line of `wnf2`:
```haskell
       Closure2 b sub' -> shift0Val (-1) (wnf2 b (shift0List 1 (a' : sub')))
```
These two shifting operations have their origin from substitution, namely
the following line from our earlier `wnf1` (with `beta` inlined for easier comparison):
```haskell
       Abs b -> wnf1 (shift 0 (-1) (subst b 0 (shift 0 1 a')))
```
A observation useful to us is that in the setting of `wnf2` the lazy
substitution `a' : sub'` substitutes **all** the free variables of `b`. Because
weak normalisation on a term never introduces new free variables, we know for
sure that the free variables in the term `wnf2 b (shift0List 1 (a' : sub')` must
all come from free variables in the lazy substitution `a' : sub'`. Therefore we
could do the final step `shift0Val (-1)` of shifting free variables by `-1`
before the `wnf2` as well. That is to say, we have equality
```
   shift0Val (-1) (wnf2 b (shift0List 1 (a' : sub')))
=  wnf2 b (shift0List (-1) (shift0List 1 (a' : sub')))
```
Then `shift0List (-1) (shift0List 1 ...)` just cancels out!
```
   wnf2 b (shift0List (-1) (shift0List 1 (a' : sub')))
=  wnf2 b (a' : sub')
```

The observation allows us to optimise `wnf2` into the following `wnf3`,
resulting a new normaliser `nf3`:
-}

wnf3 :: Tm -> Subst2 -> Val2
wnf3 (Var n) sub = sub !! n
wnf3 (Abs b) sub = Closure2 b sub
wnf3 (App f a) sub =
  let f' = wnf3 f sub
      ~a' = wnf3 a sub
  in case f' of
       Closure2 b sub' -> wnf3 b (a' : sub')
       Spine2 v as -> Spine2 v (a' : as)

nf3 :: Tm -> Tm
nf3 t = let n = fv t
        in reify2 (wnf3 t (reflect2 n))

{-
The same sanity checks again:
-}

-- >>> nf3 c2
-- \x0. \x1. x0 (x0 x1)

-- >>> nf3 (cmul `App` c2 `App` (cinc `App` c2))
-- \x0. \x1. x0 (x0 (x0 (x0 (x0 (x0 x1)))))

-- >>> nf3 (cmul `App` c2 `App` (cinc `App` c2) `App` Var 0 `App` Var 1)
-- x1 (x1 (x1 (x1 (x1 (x1 x0)))))

-- >>> nf3 (cmul `App` (cinc `App` c2) `App` (cinc `App` c2))
-- \x0. \x1. x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 x1))))))))

{-
The normaliser `nf3` is essentially already _normalisation by evaluation_ for
untyped lambda calculus. To normalise a term `t`, we first evaluate it into a
semantic domain `Subst2 -> Val2` -- note the semantic domain is `Subst2 -> Val2`
rather than just `Val2`! From the resulting value, we obtain a normal form by
reflecting the context of the term `t` into a substitution and then reifying the value.
-}

{-
Functional normalisation by evaluation
--------------------------------------------------------------------------------

I have been saying that `wnf3 t` _evaluates_ a term into a semantic domain. However,
if we are pedantic with terminology, we should only use the word the word
'evaluate' if `wnf3` is computed compositionally (i.e. `wnf3` should be a fold
of terms). Unfortunately, `wnf3` is not (at least, not on the nose) compositional
because of this line:
```haskell
       Closure2 b sub' -> wnf3 b (a' : sub')
```
where we do recursion on `b` but `b` isn't a subterm of the input `App f a`.
However, if we look more closely at `wnf3`, we see that the stem of problem is
that when `wnf3` sees a lambda abstraction, we simply stored the function body
`b` and the lazy substitution `sub` as `Closure2 b sub` without doing structural
recursion into the subterm `b`. Later, when `wnf3` uses `Closure2 b sub`,
it is used as a structural recursion `wnf3 b (a' : sub')` into the function body
`b` (with an extended substitution).  Therefore the computation of `wnf3` is in
fact compositional -- we just need to refactor `wnf3` a bit to make `wnf3 (Abs
b) sub` a function `\a. wnf3 b (a : sub)` of type `Val2 -> Val2` that performs
structural recursion into the subterm `b`.  Correspondingly, in places where we
previously do `Closure2 b sub' -> wnf3 b (a' : sub')`, we now no longer need to
do the recursion `wnf3 b` here, as this job has been done by `wnf3 (Abs b) sub`
now.

Turning `Closure2 Tm Subst2` into a genuine function `Val2 -> Val2` is the key
idea, but there is one more small change that we also need to do too. Another
place that `Closure2` is involved is when it is reified
```haskell
reify2 (Closure2 b sub) = Abs (reify2 (wnf2 b (Spine2 0 [] : shift0List 1 sub)))
```
where the free variables in the original substitution `sub` is shifted by `1`.
After refactoring `Closure2 b sub` into a function `Val2 -> Val2`, we no longer
have direct access to `sub` to do `shift0List 1 sub`. But this problem is easy
to fix: we can just make the function `Val2 -> Val2` to take an additional
integer argument and apply it to `sub`.
-}

{-
Now let's execute this refactoring. First of all, we need to change the
constructor `Closure2 Tm Subst2` from storing a function body and a lazy
substitution to a genuine function:
-}
data Val4 = Closure4 (Int -> Val4 -> Val4) | Spine4 Int (LazyList Val4)
type Subst4 = LazyList Val4

wnf4 :: Tm -> Subst4 -> Val4
wnf4 (Var n) sub = sub !! n

{-
And when `wnf4` sees a lambda abstraction, it defines a function that
structurally recurs into `b`:
-}
wnf4 (Abs b) sub = Closure4 (\i (~a) -> wnf4 b (a : shift0List4 i sub))

{-
Correspondingly, in the case of `App f a`, we just need to call the
function in the place where we previously had `Closure2 b sub' -> wnf3 b (a' : sub')`:
-}
wnf4 (App f a) sub =
  let f' = wnf4 f sub
      ~a' = wnf4 a sub
  in case f' of
       Closure4 b -> b 0 a'
       Spine4 v as -> Spine4 v (a' : as)

{-
And in reification, where we previously did
```haskell
reify2 (Closure2 b sub) = Abs (reify2 (wnf2 b (Spine2 0 [] : shift0List 1 sub)))
```
now we just need to do:
-}
reify4 :: Val4 -> Tm
reify4 (Closure4 b) = Abs $ reify4 (b 1 (Spine4 0 []))
reify4 (Spine4 v as) = foldr (\a r -> r `App` reify4 a) (Var v) as

{-
Shifting on closures needs some slight refactoring as well:
-}
shift0Val4 :: Int -> Val4 -> Val4
shift0Val4 0 v = v
shift0Val4 i (Spine4 n as) = Spine4 (n + i) (shift0List4 i as)
shift0Val4 i (Closure4 b) = Closure4 (b . (+i))

shift0List4 :: Int -> Subst4 -> Subst4
shift0List4 0 vs = vs
shift0List4 i vs = map (shift0Val4 i) vs

{-
Reflection however stays the same:
-}
reflect4 :: Int -> Subst4
reflect4 n = map (\v -> Spine4 v []) [0 .. n]

{-
And our new normaliser is again:
-}
nf4 :: Tm -> Tm
nf4 t = let n = fv t
        in reify4 (wnf4 t (reflect4 n))

{-
Just to check again that 3 times 3 is still 9:
-}
-- >>> nf4 (cmul `App` (cinc `App` c2) `App` (cinc `App` c2))
-- \x0. \x1. x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 x1))))))))

{-
After performing this refactoring, `wnf4` becomes compositional -- it is a fold
on the syntax of lambda terms. I'd like to emphasise that we have `wnf4` only
for making the hidden compositional nature `wnf3` explicit. In terms of performance,
`wnf4` makes no improvement over `wnf3`. They do normalisation in exactly the
same way. In fact, we prefer `wnf3` in practice because it doesn't need the host
language to support higher-order functions, while `wnf4` does. Therefore we can
easily port `wnf3` to languages such as C. In the literature, the way that
`wnf3` deals with lambda abstractions is called _defunctionalization_: it
implements (higher-order) functions as first-order data (a syntax tree paired
with the lazy substitution).
-}

{-
The normaliser `nf4` is a milestone of this article: it is essentially what you
would get if you extract the algorithmic content of a categorical-glueing
normalisation proof for typed calculi and erase all the type information.
The functions `shift0Val4` and `shift0List4` correspond to the presheaf actions
in the normalisation proof, and the integer arguments of those shifting
functions correspond to the weakening substitutions in the proof.

The only major difference between `nf4` and the algorithmic content of a
normalisation proof is that `nf4` only computes beta-normal forms while
normalisation proofs of typed calculi typically also do normalisation of
eta-equalities for (dependent) functions and pairs. However, this is not because
we did anything wrong -- this is just because normalising eta-equalities is
_type directed_ while here we are in the setting of an untyped language, which
doesn't give us any information telling us which variables should be
eta-expanded (this is the reason why our reflection functions look so trivial).
-}


{-
Normalisation by evaluation with de Bruijn levels
--------------------------------------------------------------------------------

Let's continue our optimisation journey. The NbE normaliser `nf3` (and `nf4`) still
has one shifting operation: when reifying a closure we had:
```haskell
reify2 (Closure2 b sub) = Abs (reify2
  (wnf2 b (Spine2 0 [] : shift0List 1 sub)))
```
The shifting operation is not cheap: it needs to traverse the whole substitution
to increment all de Bruijn indices by `1`. How can we make this faster? Lazy
tagging! Well lazy tagging will do the job (and we will do it later), but there is
in fact an extremely simple trick that would work here too.

At the moment, variables are represented as de Bruijn indices, and to bring a de
Bruijn index `v` from a context of `n` variables to a context of `n+1` variables,
we need to shift `v` to `v+1` (an `O(1)` operation). Can we find another
representation of variables so that shifting a variable from a context of `n`
variables to a context of `n+1` variables is is even better than `O(1)`? The
answer is yes! We represent a variable by an integer just like de Bruijn indices,
but count from outside in rather than inside out. For example, in the context
of `b` in the following term (assuming `b` has no free variables other than `x`,
`y`, `z`):
```
\x. \y. \z. b
```
We will represent `x` as `0`, `y` as `1`, and `z` as `2`. In comparison, de Bruijn
indices would represent `z` as `0`, `y` as `1`, and `x` as `2`. In this way,
if `b` is a lambda abstraction, `b = \w. c`, in the context of `c`, the variables
`x`, `y`, and `z` would still be `0`, `1`, and `2`. Shifting variables into the
next level is `O(0)` (i.e. an no-op)! This kind of variable representation
is sometimes called de Bruijn _levels_.

Using this idea, we can optimise `nf3` by using de Bruijn levels (instead of
de Bruijn indices) in our semantic domain. The input terms can still use
de Bruijn indices as before, and we don't need to transform them into de Bruijn
levels. Note that this optimisation is only possible because in the process of
weak normalisation (`wnf2`, `wnf3`, and `wnf4`), we always (weakly) normalises a
function body when it receives its concrete argument.  Therefore all variables
are instantiated _from outside to inside_.
-}

{-
For this optimisation, we don't need to change `wnf3`, as integer representations
of variables are only dealt in the `reify` and `reflect` functions. We change
`reify2 :: Val2 -> Tm` to the following:
-}
reify5 :: Int -> Val2 -> Tm
reify5 lvl (Closure2 b sub) = Abs (reify5 (lvl + 1) (wnf3 b (Spine2 lvl [] : sub)))
{-
Here when we reify a function body, we (lazily) substitute the current de Bruijn
level `lvl` for the newly bound variable, instead of the de Bruijn index `0` as
we did previously in `reify2`. This necessitates us to know the current level of
the input value, so `reify5` now has an additional `Int` argument `lvl`. Because
of this representation, we do no need to do `shift0List 1` to `sub` anymore.
-}

{-
Conversely, when we reify a spine back to a normal form, the output has type
`Tm`, which still use de Bruijn indices. Hence we need to convert a de Bruijn level
`v` back to a de Bruijn index `lvl - v - 1`:
-}
reify5 lvl (Spine2 v as) = foldr (\a s -> s `App` reify5 lvl a) (Var (lvl - v - 1)) as


{-
Similarly, the `reflect` function should be refactored to use de Bruijn levels as well.
Therefore we change `reflect2 n = map (\v -> Spine2 v []) [0 .. n]` to
-}

reflect5 :: Int -> Subst2
reflect5 n = map (\v -> Spine2 (n - v) []) [0 .. n]

{-
We don't need to change `wnf3` at all. Therefore our new normaliser is
-}
nf5 :: Tm -> Tm
nf5 t = let n = fv t
        in reify5 (n+1) (wnf3 t (reflect5 n))

-- >>> nf5 c2
-- \x0. \x1. x0 (x0 x1)

-- >>> nf5 (cmul `App` c2 `App` (cinc `App` c2))
-- \x0. \x1. x0 (x0 (x0 (x0 (x0 (x0 x1)))))

-- >>> nf5 ((cmul `App` c2 `App` (cinc `App` c2)) `App` Var 0 `App` Var 1)
-- x1 (x1 (x1 (x1 (x1 (x1 x0)))))

{-
The normaliser `nf5` is exactly the version of NbE that you wound find in
András Kovács's [elaboration zoo](https://github.com/AndrasKovacs/elaboration-zoo/blob/master/01-eval-closures-debruijn/Main.hs)
(which by the way is probably the best online resource for anyone who wants to
learn practical implementations of type theories). This is an efficient
normaliser of untyped lambda calculus and we have reconstructed it
by a series of optimisations from the naive applicative-order normaliser.

Reflection on NbE
--------------------------------------------------------------------------------

Now let's a good time to reflect on what we have done from the very beginning:

1. _We made the normalisation of arguments lazy_.

   Our rationale for this is that if the argument doesn't get used in the
   function body at all, we don't waste any time on normalising the argument.

2. _We switched our evaluation strategy from applicative order to what may be
called weak applicative order_.

   This strategy can be summarised as _no beta-reductions are allowed under the
   scope of a lambda abstraction, unless the lambda abstraction has no arguments_,
   or alternatively, _normalising a term to a weak normal form first before
   normalising under lambda abstractions_.

   Our rationale for this is that if we delay normalising a function body `b` until
   the bound variable `x` receives its concrete value `a`, we can skip
   normalisation of arguments `M` that are supplied to `x` but are actually not
   used by `a` when `x` becomes `a`.

3. _We used the trick of lazy tagging to implement substitutions_ to make
   substitutions fast.

4. _We optimised out shifting de Bruijn indices by some algebraic simplification
   and using de Bruijn levels in weak normal forms_.

Therefore, now we may say we understand normalisation by evaluation better!

::: {.quote}
_Normalisation by evaluation is just weak-applicative-order normalisation with
meta-level laziness, lazy substitution, and de Bruijn levels_.
:::

In fact, the version of NbE that we extract from glueing proofs is just
weak-applicative-order normalisation with lazy substitution.
-}


{-
Adversarial Exploit of NbE
--------------------------------------------------------------------------------

We shall not stop at NbE. With our knowledge gained in the reconstruction of
NbE, let's push NbE further. Recall that the reason that led to using
weak-applicative-order normalisation is that when we normalise `App f a`, the
optimal thing to do for the function `f` is to normalise it just enough to
expose its outermost lambda abstraction. However, for the argument `a` we chose
to also normalise it to expose the outermost lambda abstraction because the
argument `a` may be used as a function in the body of `f`. Our strategy for `f`
is good, but our strategy for `a` is based on the assumption that `a` will be
used as a function inside `f` later.

Of course this assumption doesn't always hold. For an extreme example, we
can have a term (with a free variable `x`)
```
(\y. x y y y y y ... y) (expensiveTm 100)
```
where the argument `expenstiveTm 100` is used as arguments in the function
body 1000000 times. NbE would be very slow on this term because the term
`expensiveTm 100` would be normalised to weak normal forms before (lazy)
substitution, and then in the function body it would be duplicated 1000000
times, and the NbE would reify the weak normal form of `expensiveTm 100`
1000000 times, one time for each occurrence of `y`.
-}

nbeAdversarialExploit :: Tm
nbeAdversarialExploit =
  Abs (foldr (\_ r -> r `App` Var 0) (Var 1) [1..1000000]) `App` expensiveTm 100

{-
On this term, our naive normaliser `nf0` takes a few seconds to compute on my laptop:
-}
-- >>> fv (nf0 nbeAdversarialExploit)
-- 0

{-
But our best NbE normaliser `nf5` takes a much longer time:
-}
-- >>> fv (nf5 nbeAdversarialExploit)
-- ProgressCancelledException

{-
Let's fix this problem in the rest of this article. The plan is simple: when we
(weakly) normalise `App f a`, we create a lazy thunk of the full normal form of
`a` and pass it to the function body of `f`. Later when we reify a value and see
a thunk of a full normal form in a spine, we can just force this thunk. In this
way, if the argument `a` is used in the function body `f` many times as
arguments in a spine, they would share the same thunk of the full normal form.
-}

{-
The plan is simple, but a technical difficulty is that when we weakly normalise
`App f a` and creates the thunk of the full normal form `a'` of `a`, the de Bruijn
level where `a'` is created will be different from the de Bruijn level where `a'`
will be used inside the function body `f`. This highlights the _disadvantage_ of
de Bruijn levels over de Bruijn indices: a closed term with de Bruijn indices is
stable if we move this term to some other locations, while a closed term with de
Bruijn levels is not stable. For example, consider the following term in the context
of one free variable `x`:
```
(\z. x (\w. z)) (\y. y y)
```
Using de Bruijn levels, the free variable `x` is represented by `0`, and `y` is
represented by `1`. However, if we substitute `\y. y y` for `z`, we obtain
`x (\w. (\y. y y))`, where `y` should be shifted to `2` (because `w` is `1`).

To fix this problem, we shall revert back to use de Bruijn indices instead of
de Bruijn levels. Recall that previously we switched from de Bruijn indices
to levels when optimising `nf3` to `nf5` to save the time of shifting de Bruijn
indices. But we mentioned that we had another solution to do this: lazy tagging.
Therefore let's first do another optimised version of `nf3` using lazy shifting
of de Bruijn indices.
-}


{-
NbE with lazy shifting of de Bruijn indices
--------------------------------------------------------------------------------

Very similar to the example of `Tree'` that we used earlier to demonstrate lazy
tagging on trees, we will need lists storing lazy tags that represent shift
offsets that are logically applied to the de Bruijn indices. For this we define
a type `SList a` of lists with lazy tags of shift indices. We will use this type
to replace `LazyList a` that we used before to represent lazy substitutions.
-}

data SList a = Nil | Cons_ Int ~a (SList a) deriving Show

{-
The type of values (weak normal forms with lazy substitutions) is the same as before,
except that we now use `SList`:
-}

data Val6 = Closure6 Tm Subst6 | Spine6 Int (SList Val6)
type Subst6 = SList Val6

{-
We will have more than one types that support index shifting, so this time let's
define a typeclass with a single member function `shift0 i v` that shifts all
de Bruijn indices in `v` by `i`. (We won't need the general case `shift x i v` of
shifting de Bruijn indices greater than or equal to `x` by `i`.)
-}
class Shift0 a where
  shift0 :: Int -> a -> a

{-
Shifting a list with lazy tags only remembers the offset `i` lazily without actually
performing the shifting operations. It is clearly `O(1)`. In contrast, previously
in `shift0List` we needed to map over the list to shift every element.
-}
instance Shift0 a => Shift0 (SList a) where
  shift0 :: Shift0 a => Int -> SList a -> SList a
  shift0 i Nil = Nil
  shift0 i (Cons_ j v vs) = Cons_ (i + j) v vs

{-
Shifting a value is easy too. For a closure, we use `shift0` for `SList` to shift
the lazy substitution, and for a spine, we shift the variable immediately and
use `shift0` for `SList` to shift the list of arguments.
-}
instance Shift0 Val6 where
  shift0 :: Int -> Val6 -> Val6
  shift0 i (Closure6 b sub) = Closure6 b (shift0 i sub)
  shift0 i (Spine6 j as) = Spine6 (i + j) (shift0 i as)

{-
To make our life later easier, this time let's use pattern synonyms of Haskell
to make lazy tagging transparent. The type `ListView a` is the functor that
captures the shape of a list (with lazy payloads but strict structure).
-}
data ListView a x = NilView | ConsView ~a x

{-
A `SList a` is logically a list of `a` viewed from the following function, where
we push the lazy tag down (c.f. the `push` function in the earlier example of
lazy tagging for binary trees):
-}
viewSList :: Shift0 a => SList a -> ListView a (SList a)
viewSList Nil = NilView
viewSList (Cons_ i v vs)
  | i /= 0 = ConsView (shift0 i v) (shift0 i vs)
  | otherwise = ConsView v vs

{-
Now we define a pattern synonym `Cons v vs`: when we use the pattern `Cons v vs`
to match again an element `as :: SList a`, it will automatically applies
`viewSList` to `as`. And when we use `Cons v vs` to construct an element of type
`SList a`, the lazy tag is going to be set to `0`. Using this pattern, the
user interface of `SList` will be exactly the same as an ordinary list.
-}
pattern Cons :: Shift0 a => a -> SList a -> SList a
pattern Cons v vs <- (viewSList -> ConsView v vs) where
  Cons ~v vs = Cons_ 0 v vs

{-
We will need some standard list-processing function for `SList` in the future,
but they are easy:
-}
lookupSL :: Shift0 a => SList a -> Int -> a
lookupSL Nil _ = error "index out of range"
lookupSL (Cons v vs) i = if i == 0 then v else lookupSL vs (i-1)

foldrSL :: Shift0 a => (a -> r -> r) -> r -> SList a -> r
foldrSL f r Nil = r
foldrSL f r (Cons v vs) = f v (foldrSL f r vs)

dropSL :: Shift0 a => Int -> SList a -> SList a
dropSL 0 as = as
dropSL i Nil = Nil
dropSL i (Cons _ as) = dropSL (i-1) as

{-
Our new weak normaliser is the same as `wnf3` except that it uses the new data
type `Val6` for values:
-}
wnf6 :: Tm -> Subst6 -> Val6
wnf6 (Var n) sub = lookupSL sub n
wnf6 (Abs b) sub = Closure6 b sub
wnf6 (App f a) sub =
  let f' = wnf6 f sub
      ~a' = wnf6 a sub
  in case f' of
       Closure6 b sub' -> wnf6 b (Cons a' sub')
       Spine6 v as -> Spine6 v (Cons a' as)

{-
Similarly `reify6` is the same as `reify2` except it uses the new data type `Val6`:
-}
reify6 :: Val6 -> Tm
reify6 (Closure6 t sub) = Abs (reify6 (wnf6 t (Cons (Spine6 0 Nil) (shift0 1 sub))))
reify6 (Spine6 v as) = foldrSL (\a r -> r `App` reify6 a) (Var v) as

{-
The function `reflect6` is the same as `reflect2`:
-}
reflect6 :: Int -> Subst6
reflect6 n = foldr (\x rs -> Cons (Spine6 x Nil) rs) Nil [0 .. n]

{-
These give us the new normaliser `nf6`, which is defunctional NbE with de Bruijn
indices (rather than levels). Because of lazy tagging, the shift operations have
`O(1)` complexity, so this version has the same asymptotic complexity as `nf5`
(the version using de Bruijn levels) on all input, but of course the constant
factor `nf6` is slightly bigger than `nf6`.
-}
nf6 :: Tm -> Tm
nf6 t = let n = fv t
        in reify6 (wnf6 t (reflect6 n))

-- >>> nf6 ((cmul `App` c2 `App` (cinc `App` c2)) `App` Var 0 `App` Var 1)
-- x1 (x1 (x1 (x1 (x1 (x1 x0)))))

-- >>> nf6 (cmul `App` c2 `App` (cinc `App` c2))
-- \x0. \x1. x0 (x0 (x0 (x0 (x0 (x0 x1)))))

{-
NbE with shared normal forms
--------------------------------------------------------------------------------

Now we have the needed infrastructure to implement our idea of improving NbE by
creating a thunk of a normal form for a function argument `a` in the case of
weakly normalising `App f a`. This thunk will be used like a _cache_: later if
the function argument `a` is duplicated in different places in the function
body, full normalisation of `a` will use the same thunk of `a`.

To do this, we change the type of `Val6` to have an additional constructor
`Cached7 t v`, where `t` is (a thunk of) the normal form of the value `v`.
We shall have the invariant that `Cached` is never nested, i.e.
`Cached _ (Cached _ _)` is not allowed.
-}

data Val7 = Closure7 Tm Subst7 | Spine7 Int (SList Val7)
          | Cached7 ~TmS Val7
type Subst7 = SList Val7

{-
Up to now, we have always used the type `Tm` of terms to store normal forms,
but this time we are going to put normal forms in lazy substitutions so we
need a new version of `Tm` that supports lazy shifting. To do this, we
modify the constructors `Abs` and `App` of `Tm` to store an additional integer
lazy tag (an integer `i` means that `shift0 i` has been logically applied to this
term):
-}

-- Terms with lazy tagging for `shift0 i`
data TmS = AbsS Int TmS | AppS Int TmS TmS | VarS Int deriving Show

{-
Shifting is again `O(1)` because we just need to accumulate it in the lazy tags:
-}
instance Shift0 TmS where
  shift0 i (AbsS j b) = AbsS (i + j) b
  shift0 i (AppS j f a) = AppS (i + j)  f a
  shift0 i (VarS j) = VarS (i + j)

{-
Similarly, shifting a value is `O(1)` because `shift0` for `SList a` and `Tms` is O(1) and
there is no two consecutive layers of `Cached`:
-}
instance Shift0 Val7 where
  shift0 :: Int -> Val7 -> Val7
  shift0 i (Closure7 b sub) = Closure7 b (shift0 i sub)
  shift0 i (Spine7 j as) = Spine7 (i + j) (shift0 i as)
  shift0 i (Cached7 n v) = Cached7 (shift0 i n) (shift0 i v)

{-
Let's define two auxiliary functions for ignoring and preparing cached normal forms
of values:
-}
ignoreCache :: Val7 -> Val7
ignoreCache (Cached7 _ v) = v
ignoreCache v = v

makeCache :: Val7 -> Val7
makeCache v@(Cached7 _ _) = v
makeCache v = Cached7 (reify7 v) v

{-
Our new weak normaliser now makes cached normal forms for function arguments
(and ignores the cached normal forms for functions):
-}
wnf7 :: Tm -> Subst7 -> Val7
wnf7 (Var n) sub = lookupSL sub n
wnf7 (Abs b) sub = Closure7 b sub
wnf7 (App f a) sub =
  let f' = wnf7 f sub
      ~a' = makeCache (wnf7 a sub)
  in case ignoreCache f' of
       Closure7 b sub' -> wnf7 b (Cons a' sub')
       Spine7 v as -> Spine7 v (Cons a' as)

{-
When we reify a value, we can use a cached normal form when there is one, otherwise
everything stays the same as `reify6`:
-}
reify7 :: Val7 -> TmS
reify7 (Closure7 t sub) = AbsS 0 (reify7 (wnf7 t (Cons (Spine7 0 Nil) (shift0 1 sub))))
reify7 (Spine7 v as) = foldrSL (\a r -> AppS 0 r (reify7 a)) (VarS v) as
reify7 (Cached7 c _) = c

{-
Reflection doesn't change at all:
-}
reflect7 :: Int -> SList Val7
reflect7 n = foldr (\x rs -> Cons (Spine7 x Nil) rs) Nil [0 .. n]

{-
With these ingredients, our new normaliser is again `reify7 (wnf7 t (reflect7
n)`.  Wait! Now `reify7` produces an element of `TmS`, a normal form containing
lazy shifts, and of course what we really want is a normal form of type `Tm`,
without any lazy shifts. Therefore we need a function `forceShifts :: TmS -> Tm`
triggers the lazy shifts stored in the resulting normal form (of type `TmS`) and
produces a normal form (of type `Tm`) that doesn't have lazy shifts.

This function doesn't sound hard, does it? It turns out this function is in fact
a bit tricky to get efficient... We don't want this `forceShifts` function
to be more expensive than the actual normalisation process itself, so the naive
implementation below isn't good enough:
-}

forceShiftsSpec :: TmS -> Tm
forceShiftsSpec (VarS v) = Var v
forceShiftsSpec (AbsS i b) =
  shift 0 i (Abs (forceShiftsSpec b))
forceShiftsSpec (AppS i f a) =
  shift 0 i (App (forceShiftsSpec f) (forceShiftsSpec a))


{-
The difficulty here is that we can't reduce the composition of two shifting
operations into a single one. We do have `shift 0 i . shift 0 j = shift 0 (i + j)`,
but unfortunately for the case of `AbsS i b` above, we have a lambda
abstraction, and `shift 0 i (Abs b)` becomes `Abs (shift 1 i b)`. In general,
the composition of two shifts isn't equal to a single shift:
```
shift x i . shift y j  =/=  shift _ _
```

At the moment, I do have an idea of implementing `forceShifts` with linear complexity,
but it needs some very fancy data structure. I don't think this idea is the right
way to do it, but for now it is what I have so let me explain it still.

The idea is that we can represent any function `S : Int -> Int` transforming de
Brujn indices as a sequence:
```
ss = [s0, s1, ..., sN]
```
which says that the variable of de Bruijn index `0` should be transformed to
`s0`, the the variable of de Bruijn index `1` should be transformed to `s1`,
etc. Therefore the function `S` is just `S v  = ss ! v`. Then the crucial
observation is that the composition `S . shift 0 i` is
```
(S . shift 0 i) v = S (v + i) = ss ! (v + i) = (drop i ss) ! v
```
That is to say, the composite function `S . shift 0 i` can be represented by
the sequence `drop i ss`.
Similarly, when we descend from the context of `Abs b` to the context of `b`, the
sequence that represents the function `S` changes from `ss` to `0 : map (1+) ss`.

Therefore to maintain a compact representation of composites of shifting functions,
we need a _persistent_ data structure that maintains a sequence of integers with
the following operations:

  1. pushing a `0` to the head of the sequence,
  2. adding 1 to all the integers in sequence,
  3. dropping the first `i` elements of the sequence for any given integer `i`.

We may call this interface 'persistent stack with _bulk_ pop'.  Our lazy tagging
list `SList Int` supports the first two operations in O(1) and the third
operation in `O(i)`. The version of `forceShifts` with `SList Int` is then as
follows.
-}

instance Shift0 Int where
  shift0 i j = i + j

forceShifts :: SList Int -> TmS -> Tm
forceShifts ss (VarS v) = Var (lookupSL ss v)
forceShifts ss (AbsS i b) =
  let ss' = Cons 0 (shift0 1 (dropSL i ss))
  in Abs (forceShifts ss' b)
forceShifts ss (AppS i f a) =
  let ss' = dropSL i ss
  in App (forceShifts ss' f)
         (forceShifts ss' a)

{-
This gives us the new normaliser, normalisation by evaluation with shared normal forms:
-}
nf7 :: Tm -> Tm
nf7 t = let n = fv t
            ss = foldr (\v rs -> Cons v rs) Nil [0 .. n]
        in forceShifts ss (reify7 (wnf7 t (reflect7 n)))

{-
Indeed, `nf7` is much faster on the `nbeAdversarialExploit` program that `nf5` was slow:
-}
-- >>> fv (nf7 nbeAdversarialExploit)
-- 0

{-
However, our `SList Int` is too slow for dropping the first `i` elements. A better
implementation of stack with bulk pop would be using balanced trees that support
`log`-time splitting, such as finger trees or red-black trees, which will give us
an `O(log i)`-time implementation of dropping `i` elements. In our concrete application,
`i` is bound by the _depth_ of lambda abstractions, so `i` will not be too big in
practice.

However, if we want to have a version of `nf7` that has better or equal
asymptotic complexity as NbE on all families of input programs, we need a linear
implementation of `forceShifts`, which needs all three operations (push, bulk
pop, add) to be `O(1)` if we implement `forceShifts` like the one above.
I do know the existence of such a data structure in the literature, namely the one
in the paper [Improved Algorithms for Finding Level Ancestors in
Dynamic Trees](https://link.springer.com/chapter/10.1007/3-540-45022-X_8) by
Alstrup and Holm. However, this data structure is rather fancy so we won't implement it
here.
-}

{-
After the initial version of this article was put online, [András
Kovács](https://andraskovacs.github.io) pointed out to me that the normaliser
`nf7` (what I called NbE with shared normal forms) is known as _strong
call-by-need evaluation_ in the literature, such as in [this
paper](https://arxiv.org/abs/2603.21949) by Biernacka et al, where 'strong'
refers to normalising under binders. Also, Biernacka et. al. implement such a
normaliser using _(freshly) named variables_ in values rather than de Bruijn
indices, because shifting names is no-op (and normal forms with named
variables can always be converted back de Bruijn indices easily).  Arguably this is a
nicer way to do `nf7` than what we did here.
However, I think the our `nf7` is cute so I decided to keep it in this article
-- it shows how far we can go with only the trick of lazy tagging and some
elementary analysis of the bottlenecks in normalisation.
-}


{-
**Editing History**

* v3 (24 Apr, 2026): this is the version you are viewing now; [source code](https://yangzhixuan.github.io/NbE.hs).

  I am an idiot -- the version of `nf7` in v2 is totally incorrect. I have reverted
  `nf7` back to the version in v1.


* v2 (23 Apr, 2026): [rendered HTML](https://yangzhixuan.github.io/NbEv2.html) and
  [source code](https://yangzhixuan.github.io/NbEv2.hs).

  1. Fix a bug in v1: although the argument `a` is marked with `~` to be lazy,
  it is passed to a _strict_ function `beta`. Now a lazy version of `betaL` is
  added.

  2. Add the discussion of doing `nf7` with named variables that András Kovács brought
  to my attention.

* v1 (22 Apr, 2026): initial version; [rendered HTML](https://yangzhixuan.github.io/NbEv1.html) and
  [source code](https://yangzhixuan.github.io/NbEv1.hs).
-}


{-
---
header-includes: |
  <style>
  .sourceCode {
    background-color: #f6f8fa;
  }
  html {
    font-family: Palatino, "Palatino Linotype", "Book Antiqua", Georgia, serif;
    font-size: 14pt;
    line-height: 1.3;
  }
  h1, h2 {
    font-family: Palatino, "Palatino Linotype", "Book Antiqua", Georgia, serif;
  }
  .quote{
    font-size: 1.2em;
    margin-left: 2em;
  }
  </style>
...
-}
