---
title: Monads vs. Waterfalls
tags: [haskell monads advent]
---

Monads vs. Waterfalls
=====================

I spent some times forging a solution to an advent of code problem ([day 17, 2018](https://adventofcode.com/2018/day/17)).
My initial algorithm was cluttered with updates to the same map, and maintaining the current position through an otherwise nice and simple recursive code.
The result feels beatiful, and deserves its own spot in the internet.

This post is a litteral haskell file. Compile and execute the [source](https://github.com/layus/layus.github.io/blob/dev/posts/2020-08-11-monads-vs-waterfall.lhs) directly with ghc.

If you do not want to read the full problem statement, you basically have to let water flow in a rocky underground.
Water can either flow freely, or remain still in pockets of rocks.

          +                      +       
                #                |     # 
     #  #       #           #  #||||   # 
     #  #  #                #  #~~#|     
     #  #  #                #  #~~#|     
     #     #                #~~~~~#|     
     #     #                #~~~~~#|     
     #######         ==>    #######|     
                                   |     
                              |||||||||  
        #     #               |#~~~~~#|  
        #     #               |#~~~~~#|  
        #     #               |#~~~~~#|  
        #######               |#######|  

We start with a few imports and a main function.

> import qualified Data.Map.Strict    as M
> import Data.Map.Strict              (Map)
> 
> import Control.Arrow                ((&&&), (***), first, second)
> import Control.Monad                (liftM2, join)
> import Data.Bool                    (bool)
> import Control.Monad.Reader         (ReaderT, runReaderT, local, ask)
> import Control.Monad.State.Strict   (State, execState, modify, get)
> import Text.Parsec                  (many, count, (<|>), char, noneOf, newline, eof)
> import Text.Parsec.String           (Parser, parseFromFile)
> import Text.Parsec.Number           (nat)
>
> main = do
>     input <- getInput "input17.txt" day17parser
>     let res  = show $ day17 input
>         res' = show $ day17bis input
>     putStrLn $ "Day 17 -- " <> res <> " -- " <> res'
>

Then four helper functions for parsing and handling input.

> -- | Get ((minX, maxX), (minY, maxY)) from a list of Pt with data
> bounds :: Ord a => [((a, a), b)] -> ((a, a), (a, a))
> bounds = join (***) (minimum &&& maximum) . unzip . map fst 
>
> parseLines :: Parser a -> Parser [a]
> parseLines p = many (p <* newline)
> 
> justNat :: Parser Int
> justNat = many (noneOf "0123456789\n") *> nat <* many (noneOf "0123456789\n")
> 
> getInput :: FilePath -> Parser a -> IO a
> getInput path p = do
>     input <- parseFromFile (p <* eof) path 
>     either (error . show) return input
> 

Then comes the real stuff. We define the `Pt` type for position in the grid,
and `Grid` to represent the ground with rocks.  Follows a parser to make sense
of the input, and one-liners to get the actual puzzle responses. We were asked
the number of tiles with water, and then the number of tiles with still water
only.

> -- Day 17
> 
> type Pt = (Int, Int) 
> type Grid = Map Pt Char
> 
> day17parser :: Parser Grid
> day17parser = M.fromList . map (\p -> (p,'#')) . concat <$> parseLines line where 
>     line = do 
>         axis <- char 'x' <|> char 'y'
>         [a, b, c] <- count 3 justNat
>         let coord = if axis == 'x' then (,) a else flip (,) a
>         return $ map coord (enumFromTo b c)
>
> -- # tiles with water, that is # non-rock tiles.
> day17 = length . filter (/= '#') . M.elems
> -- # still water tiles
> day17bis = length . filter (== '~') . M.elems

General idea
------------

We proceed by propagating the flow in different directions.
Each propagation function returns a boolean to tell whether we
overflowed in that direction.

On overflows, we propagate in other directions, or mark water still.
This is reflected by three operation. Pouring (down), filling (left and
right) and "stilling" (marking water as still).

The algorithm is a counter-intuitive we mark the current cell unconditionally and
test the next one for being free.
That is actually an invariant: always call the flow operators on a free cell.
The only rationale for this rule is to mark cells at only one place,
avoiding code duplication. You can check that there is only one location
where we write '`|`' and '`~`' in the grid.

Using monads
------------

In this code, we have to carry around the current position in the grid and the
grid itself.  Because they are used everywhere, it turns out to be simpler not
to handle them explicilty, but hide them away in the context of the
computation.  That is exactly what monads are for. Performing a computation in
a context.  In our context, we would like to have i) a mutable grid and ii) the
current position.

This is what the State and Reader monads are made for.  The state monad carries
a value that can be altered. The new value will be the only one available to
subsequent computations.

The reader monad is much like the state, except that the value can only be
read, not altered.  It is however possible to start new computations with a
different value to be read. Think of it like a scope.  All the computations in
the same scope will read the same value, and there is no way for a scope to
alter values in its parent.

That is why we define a flow as a stack of two monads: state and reader.
Monads wrap (or return) a value. In this case, our computations need
to return whether they overflowed. So a Bool will do.

> -- | A Flow is a local action on the grid.
> type Flow = ReaderT Pt (State Grid) Bool
> --          ^           ^           ^- A flow returns a boolean to tell whether it overflowed.
> --          |           `- The grid is a `modify`able implicit context.
> --          `- We can always `ask` our current position.

As with every monads, we need to compose these actions.  Usually, the result of
a monadic computation can be fed into another.  In this case, we would rather
use the result to know if we should perform further actions.

For example, we would like to perform the fill action only when the pouring
overflowed.  That is why we define custom operators for monads on booleans.

The definition is obscure. It ensures that the operators fail fast, and
only perform the second computation when the first one is not enough to
determine the result.  `<&&> `is the non fail-fast exception. Both computations
are performed before collecting the results.

> infixl 1 <&&>, ||>, &&>
> (&&>), (||>), (<&&>) :: (Monad m) => m Bool -> m Bool -> m Bool
> a  &&> b = a >>= bool (return False) b
> a  ||> b = a >>= bool b (return True)
> a <&&> b = liftM2 (&&) a b

So here is the real beast. We start pouring in a monadic context.  The state is
initialized with `grid`, and the reader with `(500, minY)`, the initial water
source.

> flow :: Grid -> Grid
> flow grid = flip execState grid $ flip runReaderT (500, minY) $ pour
>   where
>     ((minX, maxX), (minY, maxY)) = bounds $ M.toList grid
>     (left, right, down) = (first (subtract 1), first (+1), second (+1))

Generic, higher-order test for the content of a cell. You pass the action you
want to perform when the current cell is free and get back an action that
returns the overflow status.  `ask` is the way to obtain the current position
from the reader monad.

>     onFree :: Flow -> Flow
>     onFree action = ask >>= \(x, y) ->
>         if y > maxY || x > maxX+10 || x < minX-10
>         then return False -- border never overflows
>         else M.lookup (x, y) <$> get >>= \cell ->
>             case cell of
>                 Just '|' -> return False
>                 Just _   -> return True
>                 Nothing  -> action

Then we define two more helpers functions.

`set` writes its argument `c` in the current cell.  Because both the grid and
the position are in the context, we only need to take `c` as parameter.
`ask` gives the current position, which is in turn used to `modify` the grid.
`modify` alters the state based on the provided `Grid -> Grid` function.

>     set :: Char -> ReaderT Pt (State Grid) ()
>     set c = ask >>= \pos -> modify (M.insert pos c)

`sides` performs a flow operation in both directions (left and right).
It overflows when both operations overflow.

>     sides op = op left <&&> op right

Part 1: Pour water below
------------------------

Pouring water down may have three outcomes.  When it flows freely, nothing more has
to be done.  On overflow below, we need to propagate on the sides.  If both sides also
also overflow, we just mark the water as still on both sides.

> 
>     pour :: Flow
>     pour = do
>         -- Set current cell to '|'
>         set '|'
>         -- `local` executes the action with a position modified by its first argument.
>         -- This new position is only visible to the action passed as second argument.
>         local down $
>             -- The action (executed only when the cell is free) consists of
>             -- pouring, filling and stilling in sequence, stopping when no
>             -- overflow occurs.
>             onFree (pour &&> sides fill &&> sides still)

Introducing an alias like `onOverflow = (&&>)`{.haskell} could improve the wording.
The last line would become ```onFree (pour `onOverflow` sides fill `onOverflow` sides still)```{.haskell}.

Part2: Filling on the sides
---------------------------

Filling is a symmetrical operation that needs to happen to the left and to the
right of the current overflowing position.  Overflows happen when both left and
right filling operations overflow. It has to be called with `sides` as we did
in `pour`.

`fill` is a one-liner. Move to the next position and try to pour when free.
On overflow, continue filling further in the same direction.

Notice that the argument `dir` represents the direction of pouring. It is a
function `Pt -> Pt`{.haskell} that modifies a position.

>     fill dir = local dir $ onFree (pour &&> fill dir)


Part 3: Marking water still
---------------------------

This operation is simpler because we expect no overflows, and only work
sideways, never downwards.  Again, the operation is symmetrical, so we split it
in two `still` operations, in each direction.

Compared to above, we are no more interested in empty cells, but in non-still
cells. We continue propagating as long as the current cell is not a rock ('`#`').
By construction, we only encounter '`|`' and '`#`'. Empty or still cells are
impossible.

>     still dir = do
>         set '~'
>         local dir (isRock ||> still dir)
>       where
>         -- cell cannot be free, action is irrelevant.
>         isRock = onFree undefined

That's all for the code.

The code is dense. It means that it requires some concentration to break
down the instructions. But overall, I love the way the essential operations
appear as first class citizens.  Solving the problem is a matter of chaining (`&&>`)
actions that

  - `set` values;
  - move around (`local`);
  - `pour`, `fill` and `still` when `on free` cells.

