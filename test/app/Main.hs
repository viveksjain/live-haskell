{-# LANGUAGE RankNTypes, TupleSections #-}

import Control.Applicative
import Data.Functor.Identity
import Data.Char

type Lens s t a b = forall f. Functor f =>
                    (a -> f b) -> s -> f t

over :: Lens s t a b -> (a -> b) -> s -> t
over l f s = runIdentity (l (Identity . f) s)

view :: Lens s t a b -> s -> a
view l s = getConst (l Const s)

_1 :: Lens (a,b) (c,b) a c
_1 f (a,b) = (,b) <$> f a

_2 :: Lens (a,b) (a,c) b c
_2 f (a,b) = (a,) <$> f b

_head :: Lens [a] [a] a a
_head f (a:as) = (:as) <$> f a

test = test2 ("foo",True)

test2 = view (_head . _1)

main = print test