module State (State, get, set, run) where

import Control.Monad(liftM, ap)

type Context a r = (a, r)

newtype State a r = State (a -> Context a r)

run :: State a r -> a -> Context a r
run (State f) context = f context

get :: () -> State a a
get () = State (\ value -> (value, value) )

set :: a -> State a ()
set value = State (\ _ -> (value, ()) )

instance Functor (State a) where
    fmap = liftM

instance Applicative (State a) where
    pure =
        \ content -> State (\ context -> (context, content) )

    (<*>) = ap

instance Monad (State a) where
    return = pure
        
    state >>= f =
        State (\ context ->
            let (context', content') = run state context
            in run (f content') context' )
