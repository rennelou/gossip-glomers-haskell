module State (State(..), Action(..), run) where

import Control.Monad(liftM, ap)

data State a r = State { context :: a, content :: r }

newtype Action a r = Action (a -> State a r)

run :: Action a r -> a -> State a r
run (Action f) context = f context

instance Functor (Action a) where
    fmap = liftM

instance Applicative (Action a) where
    pure =
        \ content -> Action (\ context -> State { context = context, content = content })

    (<*>) = ap

instance Monad (Action a) where
    return = pure
        
    action >>= f = 
        Action (\ context ->
            let State { context = context', content = content'} = run action context
            in run (f content') context' )
