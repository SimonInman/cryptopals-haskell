--state monad example
import Control.Monad.State
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push 3 stack
    (a , newStack2) = pop newStack1
    in pop newStack2

--implementation of state monad
--newtype State s a = State { runState :: s -> (a,s) }
  
--instance Monad (State s) where
--  return x = State $ \s -> (x,s)
--  (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                      (State g) = f a
--                                  in g newState

pop2 :: State Stack Int
pop2 = state $ \(x:xs) -> (x,xs)

push2 :: Int -> State Stack ()
push2 a = state $ \xs -> ((),a:xs)

stackManip2 :: State Stack Int
stackManip2 = do
    push2 3
    a <- pop2 
    pop2

stackStuff :: State Stack ()
stackStuff = do
    a <- pop2
    if a == 5
        then push2 5
        else do
            push2 3
            push2 8
