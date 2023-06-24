module State (
      State
    , get
    , put
    , run
    , ExceptState
    , runExceptState ) where

import Control.Monad.Except

type Context s r = (r, s)

------------------- Monad State -------------------------------

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

-------------------- ExceptTState ---------------------

type ExceptState s r = ExceptT String (State s) r

runExceptState :: ExceptT String (State s) r -> s -> (Either String r, s)
runExceptState execptTState = run (runExceptT execptTState)

----------------------------------------------------------------