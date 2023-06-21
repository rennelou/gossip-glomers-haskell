module State (State(..), Action(..), run) where

import Control.Monad(liftM, ap)

data State a r = State { state :: a, content :: r }

newtype Action a r = Action (a -> State a r)

run :: Action a r -> a -> State a r
run (Action f) state = f state

instance Functor (Action a) where
    fmap = liftM

instance Applicative (Action a) where
    pure =
        \ content -> Action (\ state -> State { state = state, content = content })

    (<*>) = ap

instance Monad (Action a) where
    return = pure
        
    action >>= f = 
        Action (\ state ->
            let State { state = state', content = content'} = run action state
            in run (f content') state' )
