module State (Context(..), State(..), run) where

import Control.Monad(liftM, ap)

data Context a r = Context { context :: a, content :: r }

newtype State a r = State (a -> Context a r)

run :: State a r -> a -> Context a r
run (State f) context = f context

instance Functor (State a) where
    fmap = liftM

instance Applicative (State a) where
    pure =
        \ content -> State (\ context -> Context { context = context, content = content })

    (<*>) = ap

instance Monad (State a) where
    return = pure
        
    state >>= f =
        State (\ context ->
            let Context { context = context', content = content'} = run state context
            in run (f content') context' )
