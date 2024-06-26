{-# LANGUAGE DeriveDataTypeable #-}

module ElmFormat.AST.PublicAST.MaybeF where


import Data.Coapplicative
import Data.Data

data MaybeF f a
    = JustF (f a)
    | NothingF a
    deriving (Data, Functor)

instance Prelude.Foldable f => Prelude.Foldable (MaybeF f) where
    foldMap f (JustF fa) = Prelude.foldMap f fa
    foldMap f (NothingF a) = f a

instance Traversable f => Traversable (MaybeF f) where
    traverse f (JustF fa) = JustF <$> traverse f fa
    traverse f (NothingF a) = NothingF <$> f a

instance Coapplicative f => Coapplicative (MaybeF f) where
    extract (JustF fa) = extract fa
    extract (NothingF a) = a

maybeF :: (a -> x) -> (f a -> x) -> MaybeF f a -> x
maybeF fromA _ (NothingF a) = fromA a
maybeF _ fromFa (JustF fa) = fromFa fa
