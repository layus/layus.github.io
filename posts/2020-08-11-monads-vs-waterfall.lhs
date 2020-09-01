---
title: Monads vs. Waterfalls
tags: [haskell monads advent]
---

Monads vs. Waterfalls
=====================

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
> -- Generic
> 
> type Pt = (Int, Int) 
> type Grid = Map Pt Char
> 
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
> -- Day 17
> 
> day17parser :: Parser Grid
> day17parser = M.fromList . map (\p -> (p,'#')) . concat <$> parseLines line where 
>     line = do 
>         axis <- char 'x' <|> char 'y'
>         [a, b, c] <- count 3 justNat
>         let coord = if axis == 'x' then (,) a else flip (,) a
>         return $ map coord (enumFromTo b c)
> 
> showRocks :: Grid -> String
> showRocks grid = unlines [ [ M.findWithDefault ' ' (x, y) grid 
>                            | x <- [minX .. maxX] ] 
>                          | y <- [minY .. maxY] ]
>   where ((minX, maxX), (minY, maxY)) = bounds $ M.toList grid
> 
> day17 = length . filter (/= '#') . M.elems
> day17bis = length . filter (== '~') . M.elems

General idea
============

We proceed by propagating the flow in different directions.
Each propagation function returns a boolean to tell whether we
overflowed in that direction.

On overflows, we propagate in other directions, or mark water still.
This is reflected by three operation. Pouring (down), filling (left and
right) and "stilling" (marking water as still).

The algorithm is a bit convoluted because we mark the previous cell, and
test the next one for being free. We constrain ourselves to invoke
operations on free cells.
The only rationale for this rule is to mark cells at only one place,
avoiding code duplication. You can check that there is only one location
where we insert '|' and '~' in the grid.

> basicFlow :: Grid -> Grid
> basicFlow grid = snd $ pour (500, minY) grid
>   where
>     ((minX, maxX), (minY, maxY)) = bounds $ M.toList grid
>     (left, right) = (first (subtract 1), first (+1))
> 
>     isFree :: Pt -> Grid -> Maybe Bool
>     isFree (x, y) grid =
>         if y > maxY || x > maxX+10 || x < minX-10
>         then Just False -- border never overflows
>         else case M.lookup (x, y) grid of
>                 Just '|' -> Just False -- no overflow
>                 Just _   -> Just True  -- overflow ('~', '#')
>                 Nothing  -> Nothing    -- empty cell, should propagate

Part 1: Pour water below  

Mark the current cell, and test the one below.  When free, move to our helper
method pour'.

>     pour :: Pt -> Grid -> (Bool, Grid)
>     pour pos grid = case isFree npos grid of
>         Just r -> (r, markedGrid)
>         Nothing -> pour' npos markedGrid
>       where
>         markedGrid = M.insert pos '|' grid
>         npos = second (+1) pos

Pouring water may have three outcomes.  When it flows freely, nothing more has
to be done.  On overflow below, we need to propagate on the sides.  If that
also overflows, we just mark the water on both sides as still.

Thanks to haskell lazyness, we can efficiently specify all the computations,
and only use the ones we really need.

>     pour' :: Pt -> Grid -> (Bool, Grid)
>     pour' pos markedGrid
>         | not overflows = (False, filledGrid)
>         | not stilled   = (False, overflGrid)
>         | otherwise     = (True,  stillGrid )
>       where
>         (overflows, filledGrid) = pour       pos markedGrid
>         (stilled,   overflGrid) = fillSides  pos filledGrid
>         (           stillGrid ) = stillSides pos overflGrid

Part2: Filling on the sides.

Filling is a symmetrical operation that needs to happen to the left and to the
right of the current overflowing position.  Overflows happen when both left and
right filling operations overflow.  To obtain the right result, we need to
chain the grids in the two calls.

>     fillSides :: Pt -> Grid -> (Bool, Grid)
>     fillSides pos baseGrid = (overflowRight && overflowLeft, fullGrid)
>       where
>         (overflowRight, halfGrid) = fill right pos baseGrid
>         (overflowLeft,  fullGrid) = fill left  pos halfGrid

Filling itself is just pouring on the side of the position.  The side itself
being defined by `dir`, the current direction in which we are filling.  The
operation occurs in two parts.  First, we test if the side position is free...

>     fill :: (Pt -> Pt) -> Pt -> Grid -> (Bool, Grid)
>     fill dir pos grid = let npos = dir pos in
>         case isFree npos grid of
>             Just r -> (r, grid)
>             Nothing -> fill' dir npos grid

... and then pour at that location.  On overflow, we continue filling in the
same direction.

>     fill' :: (Pt -> Pt) -> Pt -> Grid -> (Bool, Grid)
>     fill' dir pos baseGrid
>         | not overflowsDown = (False,        downGrid)
>         | otherwise         = (overflowsDir, dirGrid )
>       where
>         (overflowsDown, downGrid) = pour     pos baseGrid
>         (overflowsDir,  dirGrid ) = fill dir pos downGrid

Part 3: Marking water still.

This operation is simpler because we expect no overflows, and only work
sideways, never downwards.  Again, the operation is symmetrical, so we split it
in two `still` operations, in each direction.

>     stillSides :: Pt -> Grid -> Grid
>     stillSides pos = still right pos . still left pos

Compared to above, we are no more interested in empty cells, but in non-still
cells. We continue propagating as long as the current cell is not a rock ('#').
By construction, we only encounter '|' and '#'. Empty or still cells are
impossible.

>     still :: (Pt -> Pt) -> Pt -> Grid -> Grid
>     still dir pos grid = case isFree npos grid of
>         Just False -> stillGrid
>         _ -> markedGrid
>       where 
>         markedGrid = M.insert pos '~' grid
>         stillGrid = still dir npos markedGrid
>         npos = dir pos

That's it. But this code is quite tedious.

We have to carry around the position (`pos`/`npos`: 30 occurrences) in all our
functions.  The same goes for the grid (`grid`/´*Grid´: 46 occurences) where we
also have to find names for each intermediate results.  That's because haskell
variables are immutable.

This code can be simplified using monads. For our purpose, a monad represents a
context in which computations occur.  In our context, we would like to have i)
a mutable grid and ii) the current position.

This is what the State and Reader monads are made for.  The state monad carries
a value that can be altered. The new value will be the only one available to
subsequent computations.

The reader monad is much like the state, except that the value can only be
read, not altered.  It is however possible to start new computations with a
different value to be read. Think of it like a scope.  All the computations in
the same scope will read the same value, and there is no way to make values of
sub-scopes available to their parent.

That is why we define a flow like as a monad with two contexts. A state and a
reader.  Monads wrap (or return) a value. In this case, our computations need
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

The definition is a bit obscure. It ensures that the operators fail fast, and
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

Simmilar to isFree. But turned into an action.  You pass the action you want to
perform when the current cell is free and get back an action that returns the
overflow status.  `ask` is the way to obtain the current position
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

Then we define two helpers functions.

`set` writes its argument `c` in the current cell.  Because both the grid and
the position are in the context, we only need to take `c` as parameter.
`ask` gives the position, which is in turn used to `modify` the grid.
`modify` alters the state based on the provided `Grid -> Grid` function.

>     set :: Char -> ReaderT Pt (State Grid) ()
>     set c = ask >>= \pos -> modify (M.insert pos c)

`sides` performs a flow operation in both directions (left and right).
It overflows when both operations overflow.

>     sides op = op left <&&> op right

After these, we find the simplified `pour`, `fill` and `still` functions.

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

`fill` is a one-liner. Move to the next position and try to pour when free.
On overflow, continue filling further in the same direction.

>     fill dir = local dir $ onFree (pour &&> fill dir)

Stilling is a bit different.  No operation is performed when the cell is free.
It cannot be free anyway.  Fill with still water until we reach a rock

>     still dir = do
>         set '~'
>         local dir (isRock ||> still dir)
>       where
>         -- cell cannot be free, action is irrelevant.
>         isRock = onFree undefined

Done.


Both codes follow the same algorithm. Under the hood they perform the same
operations in the same order.  The second version is however way more concise.
There are 33% less words in the second version. 50% if you omit the declaration
of the Flow type and the binary operators.  Considering that most of the code
resides in the isFree/onFree function and above. The reduction is even larger
on the three main operations.

The second code is more dense. It means that it takes a bit more concentration to
break down the instructions. But overall, I love the way the essential
operations now appear first class.
Solving the problem is a matter of chaining (`&&>`) actions that

  - `set` values;
  - move around (`local`);
  - `pour`, `fill` and `still` when `on free` cells.

