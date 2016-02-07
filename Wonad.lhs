http://blog.sigfpe.com/2007/04/trivial-monad.html

> module Wonad where
> import Control.Monad (liftM, ap)

> data W a = W a deriving (Eq, Show)

See https://ghc.haskell.org/trac/ghc/wiki/Migration/7.10

> instance Applicative W where
>   pure = W
>   (<*>) = ap

> instance Functor W where
>   fmap = liftM

> instance Monad W where
>   (W x) >>= f = f x
>   return = pure

> bind = flip (>>=)

In fact, bind is more general than fmap: fmap can be defined in terms of bind:

> fmap' :: (a -> b) -> (W a -> W b)
> fmap' f = bind (return . f)

> f :: Int -> W Int
> f x = W (x+1)

> a :: W Int
> a = W 1
> b = fmap (+1) a
> c = bind f (f 1)
> d = bind f (bind f (f 1))

(1) define a function g :: Int -> W Int -> W Int so that g x (W y) = W (x+y).
Obviously that definition won't do - the left hand side has a W y pattern so
it's actually unwrapping. Rewrite this function so that the only unwrapping
that happens is carried out by bind.

> g :: Int -> W Int -> W Int
> g x w = w >>= return . (+x)

g x w = w >>= (\y -> return (x+y))
g x = bind (\y -> return (x+y) )

(2) define a function h :: W Int -> W Int -> W Int so that
h (W x) (W y) = W (x+y). Again, no unwrapping.

> h :: W Int -> W Int -> W Int
> h w m = w >>= (\x -> g x m)

h w m = bind (\x -> g x m) w

(3) Prove the three monad laws for W. (This should be almost trivial)
https://wiki.haskell.org/Monad_Laws

Left identity:
return a >>= f ≡ f a

> leftId_L :: W Int
> leftId_L = (return 0) >>= return

> leftId_R :: W Int
> leftId_R = return 0

> leftId = leftId_L == leftId_R

Right identity:
m >>= return ≡ m

> rightId_L :: W Int
> rightId_L = (W 0) >>= return

> rightId_R :: W Int
> rightId_R = W 0

> rightId = rightId_L == rightId_R

Associativity:
(m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

> assoc_l :: W Int
> assoc_l = (W 0) >>= return >>= return

> assoc_r :: W Int
> assoc_r = (W 0) >>= (\x -> return x >>= return)

> assoc = assoc_l == assoc_r

(4) We can't completely unwrap things using the monad API. But we can unwrap
one layer from things that are wrapped twice. So here's a nice puzzle: define a
function join :: W (W a) -> W a using the Monad API and no explicit unwrapping.

> join :: W (W a) -> W a
> join m = m >>= id

join w = w >>= (\m -> m >>= return)
