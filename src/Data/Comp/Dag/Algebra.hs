{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Dag.Algebra
-- Copyright   :  (c) 2014 Patrick Bahr, Emil Axelsson
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@di.ku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module implements catamorphisms on dags.
--
--------------------------------------------------------------------------------


module Data.Comp.Dag.Algebra
    (
      cataDag
    , cataMDag
    , cataDag'
    , cataMDag'
    , module I
    ) where

import Data.Comp.Dag
import Data.Comp.Dag.Internal
import Data.Comp.Term
import Data.Comp.Algebra as I
import qualified Data.IntMap as IntMap


cataDag :: forall f a . Functor f => Alg f a -> Dag f -> a
cataDag f Dag {root, edges} = f $ fmap run root where
    run :: Context f Node -> a
    run (Term t) = f $ fmap run t
    run (Hole n) = f . fmap run $ edges IntMap.! n

cataMDag :: forall f a m . (Traversable f, Monad m) => AlgM m f a -> Dag f -> m a
cataMDag f Dag {root, edges} = f =<< mapM run root where
    run :: Context f Node -> m a
    run (Term t) = f =<< mapM run t
    run (Hole n) = f =<< mapM run (edges IntMap.! n)

cataDag' :: forall f a . Functor f => Alg f a -> Dag' f -> a
cataDag' f Dag' {root', edges'} = f $ fmap run root' where
    run :: Node -> a
    run n = f . fmap run $ edges' IntMap.! n

cataMDag' :: forall f a m . (Traversable f, Monad m) => AlgM m f a -> Dag' f -> m a
cataMDag' f Dag' {root', edges'} = f =<< mapM run root' where
    run :: Node -> m a
    run n = f =<< mapM run (edges' IntMap.! n)

