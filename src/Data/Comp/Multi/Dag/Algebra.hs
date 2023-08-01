{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Dag.AG
-- Copyright   :  (c) 2014 Patrick Bahr, Emil Axelsson
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@di.ku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module implements catamorphisms on dags.
--
--------------------------------------------------------------------------------


module Data.Comp.Multi.Dag.Algebra
    (
      cataDag
    , cataMDag
    , cataDag'
    , cataMDag'
    , module I
    ) where

import Data.Comp.Multi
import Data.Comp.Multi.Dag
--import Data.Comp.Multi.Dag.Internal
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Algebra as I
import qualified Data.Dependent.Map as M


cataDag :: forall f a . HFunctor f => Alg f a -> Dag f :-> a
cataDag f Dag {root, edges} = f $ hfmap run root where
    run :: Context f Node :-> a
    run (Term t) = f $ hfmap run t
    run (Hole n) = f . hfmap run $ edges M.! n

cataMDag :: forall f a m . (HTraversable f, Monad m) => AlgM m f a -> NatM m (Dag f) a
cataMDag f Dag {root, edges} = f =<< hmapM run root where
    run :: NatM m (Context f Node) a
    run (Term t) = f =<< hmapM run t
    run (Hole n) = f =<< hmapM run (edges M.! n)

cataDag' :: forall f a . HFunctor f => Alg f a -> Dag' f :-> a
cataDag' f Dag' {root', edges'} = f $ hfmap run root' where
    run :: Node :-> a
    run n = f . hfmap run $ edges' M.! n

cataMDag' :: forall f a m . (HTraversable f, Monad m) => AlgM m f a -> NatM m (Dag' f) a
cataMDag' f Dag' {root', edges'} = f =<< hmapM run root' where
    run :: NatM m Node a
    run n = f =<< hmapM run (edges' M.! n)
