{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) ::
    (a -> b)
    -> Compose f g a
    -> Compose f g b
  fn <$> (Compose f) =
    Compose ((fn <$>) <$> f)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure ::
    a -> Compose f g a
  pure a =
    Compose (pure (pure a))
  (<*>) ::
    Compose f g (a -> b)
    -> Compose f g a
    -> Compose f g b
  (Compose fns) <*> (Compose f) =
    Compose (lift2 (<*>) fns f)

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
