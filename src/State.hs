module State (
      State
    , get
    , put
    , run
    , StateT(..)
    , getT
    , putT
    , liftState ) where

import Control.Monad(liftM, ap)
import Control.Monad.Trans.Class

type Context s r = (r, s)

-------------- Monad State -----------------------------------

newtype State s r = State (s -> Context s r)

run :: State s r -> s -> Context s r
run (State f) context = f context

get :: State s s
get = State (\context -> (context, context) )

put :: s -> State s ()
put value = State (\_ -> ((), value) )

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure content = State (\context -> (content, context) )

    (<*>) = ap

instance Monad (State s) where
    return = pure
        
    state >>= f =
        State (\context ->
            let (content', context') = run state context
            in run (f content') context' )

----------------------------------------------------------------

-------------------- Monad Transform State ---------------------

newtype StateT s m r = StateT { runStateT :: s -> m (r, s) }

instance (Monad m, Functor m) => Functor (StateT s m) where
    fmap = liftM

instance (Monad m, Functor m) => Applicative (StateT s m) where
    pure content = StateT $ \context -> pure (content, context)

    (<*>) = ap

instance (Monad m, Functor m) => Monad (StateT s m) where
    return = pure

    StateT scontent >>= f = 
        StateT $ \context -> do
          (content, context') <- scontent context
          runStateT (f content) context'

instance MonadTrans (StateT s) where
  lift ma =
    StateT $ \context -> do
      a <- ma
      return (a, context)

getT :: (Monad m) => StateT s m s
getT = StateT $ \context -> return (context, context)

putT :: (Monad m) => s -> StateT s m ()
putT value = StateT $ \_ -> return ((), value)

liftState :: (Monad m) => State s r -> StateT s m r
liftState scontent = StateT $ \context -> return (run scontent context)

----------------------------------------------------------------