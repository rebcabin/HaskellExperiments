import Test.HUnit
import Test.QuickCheck
import Lib
import Control.Arrow
import Control.Monad

-- Really basic stuff to firm up my understanding, proficiency, and fluency.

--------------------------------------------------------------------------------
f :: Num a => a -> a
f x = 2 * x

g :: Num a => a -> a
g x = x ^ 5

someTests = do
  putStrLn    "warmups"
  assertEqual "function-call syntax"   (f (g 42)) ( (f . g) 42 )
  assertEqual "precedence"             (f (g 42)) (  f $ g  42 )

--------------------------------------------------------------------------------

internalEtaStumbleOn = do
  putStrLn    "Eta Tests"
  assertEqual "no eta"                 32.0 (2 ^^ 5)
  assertEqual "regular eta"            ((2 ^^) 5) ((\x -> 2 ^^ x) 5)
  assertEqual "internal eta"           ((^^ 5) 2) ((\x -> x ^^ 5) 2)
  assertEqual "prefix notation"        32.0 ((^^) 2 5)

  assertEqual "just dot"               (f (g 42)) ((f . g) 42)
  assertEqual "eta on dot"             (f (g 42)) ((f .) g 42)
  assertEqual "co-eta on dot"          (f (g 42)) ((. g) f 42)

  assertEqual "just arrow"             (f (g 42)) ((g >>> f) 42)
  assertEqual "eta on arrow"           (f (g 42)) ((g >>>) f 42)
  assertEqual "co-eta on arrow"        (f (g 42)) ((>>> f) g 42)

--------------------------------------------------------------------------------

data Point = Point { _x, _y   :: Double } deriving Show
data Mario = Mario { _location :: Point } deriving Show

player1 = Mario (Point 0 0)

someCompTests = do
  putStrLn    "Composition"
  assertEqual "nested record access"   0 ( _x ( _location  player1 ) )
  assertEqual "dollar syntax"          0 ( _x $ _location  player1)
  assertEqual "dot syntax"             0 ((_x . _location) player1)

--------------------------------------------------------------------------------

mm = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]]
sqr = \x -> x * x

someCompCompTests = do
  putStrLn    "Compposition of Comppositions"
  assertEqual "dotted fmap"            (((fmap . fmap) sqr) mm)
                                       (fmap (\xs -> fmap sqr xs) mm)

  assertEqual "eta inside"             (((fmap . fmap) sqr) mm)
                                       (fmap (fmap sqr) mm)

  assertEqual "replace with dot"       (((fmap . fmap) sqr) mm)
                                       ( (fmap . fmap) sqr  mm)

  assertEqual "prefix notation"        (((fmap . fmap) sqr) mm)
                                       ((.) fmap fmap sqr mm)

  assertEqual "(.)(.)"                 "6"
                                       (((.).(.)) show (+) 2 4)

  assertEqual "(.)(.)(.)"              "12"
                                       (((.).(.).(.))
                                         show
                                         (\x y z -> x + y + z)
                                         2 4 6)
{-

In https://github.com/ekmett/lens/wiki/Derivation, we see some types for
composition of compositions: (.).(.), (.).(.).(.), and so on. Let's prove that
the type of (.).(.) is (a -> b) -> (c -> d -> a) -> c -> d -> b, as stated in
the site. We'll stick with prefix notation, meaning that we want the type of
(.)(.)(.).

Recall the type of composition. This should be intuitive:

  (.) :: (b -> c) -> (a -> b) -> a -> c                                      [1]

Apply this to a function of type b -> c. Be concrete so that we don't get fooled
by renaming and unification in the type-checker. Start with a function of type
(b = Int) -> (c = String).

  ((\x -> show x) :: Int -> String) :: Int -> String                         [2]

  ( (.) ((\x -> show x) :: Int -> String) ) :: (a -> Int) -> a -> String     [3]

This is good: the combination ((.) ...) is waiting for an (a -> Int); when it
gets one, it will yield an (a -> String), as expected.

What if we feed (.) to (.)? Look at the type of the second (.) and map its
components to (Int -> String) from the first example.

  (.) :: (b -> c) -> (a -> b) -> a -> c                                      [4]

         `--.---'    `-------.--------'

  ~~~ ::   Int    ->      String

Replace Int with (b -> c) and String with (a' -> b) -> a' -> c in equation 3,
(the two a's are different) we get an expected type for (.) (.):

              (a ->   Int)  -> a ->        String

                    ,--'--.         ,---------'--------.

  ((.)(.)) :: (a -> b -> c) -> a -> (a' -> b) -> a' -> c                     [5]

does GHCi agree?

  ((.)(.)) :: (a1 -> b -> c) -> a1 -> (a2 -> b) -> a2 -> c                   [6]

Yup, with a <- a1 and a' <- a2. This is hard to intuit, so go back to concrete
types. ((.)(.)) is waiting for a another function. Remember that ((.) ...) is
waiting for an (a -> Int), and, when it gets one, will yield a (a -> String).
Our new thing, ((.)(.)), must be waiting for an (a1 -> b -> c), and, when it
gets one, will yield an a1 -> (a2 -> b) -> a2 -> c. Try giving it an Int ->
String -> Bool, that is, a function where a1 == Int, b == String, and c == Bool.
One such function is:

   (\x -> \y -> show x == y) :: Int -> String -> Bool                        [7]

Test it in the REPL as follows:

  ((\x -> \y -> show x == y) :: Int -> String -> Bool) 42 "42" ~~> True
  ((\x -> \y -> show x == y) :: Int -> String -> Bool) 42 "foobar" ~~> False

What happens when ((.)(.)) gets an Int -> String -> Bool?

  ((.)(.)) ((\x -> \y -> show x == y) :: Int -> String -> Bool)              [8]
    :: Int -> (a -> String) -> a -> Bool

which is a1 -> (a2 -> b) -> a2 -> c when a1 == Int, b == String, c == Bool, and
a2 is renamed (by the type-checker) to a. Score one for equational reasoning!

Ok, this bad boy, ((.)(.)), from equation 6, is waiting for a function of type
a1 -> b -> c. There is one laying around: (.) --- after some renaming:

  (.) :: (b -> c) -> (a -> b) -> a -> c

         `---.--'    `---.--'    `--.-'

            a1    ->     b    ->    c                                        [9]

If ((.)(.)) gets a (.) from equations 6 and 9, we expect to see

     a1    -> (a2 ->     b    ) -> a2 ->     c

  `---.--'           `---.--'            `---.--'

  (b -> c) -> (a2 -> (a -> b) ) -> a2 -> (a -> c)                           [10]

What does GHCi say?

  (b -> c) -> (a1 -> a2 -> b  ) -> a1 -> a2 -> c                            [11]

This works when we replace or rename, in equation 11, a1 becoming a2 and a2
becoming a.

Now intuit the pattern: A composition takes two 1-functions and returns a
1-function, where a 1-function takes one argument and produces a value. The
composition of compositions, namely (.)(.)(.) or (.).(.), takes a 1-function and
a 2-function and produces a 2-function. Call that a cc. Going further, the
composition of composition and cc, namely ccc = ((.)(.)((.)(.)(.))), takes a
1-function and a 3-function, and returns a 3-function. Remove some parentheses
and nesting by noting that ccc = (.).(.).(.), remembering that prefix notations
fix to the left (have left fixity). Keep composing composition with cc..c's,
adding one more arity to the second argument and the result:
((.)(.)((.)(.)((.)(.)(.)))) is the same as (.).(.).(.).(.).

-}

--------------------------------------------------------------------------------

-- See [McBride & Paterson, Applicative Programming with Effects]
-- (www.staff.city.ac.uk/~ross/papers/Applicative.html)

{-
Analysis of types:

Following, from the paper, the definition of `sequence` in terms of `ap` from
`Control.Monad`:

  sequence :: [IO a] -> IO [a]
  sequence [] = return []
  sequence (c : cs) = return (:) `ap` c `ap` sequence cs

Decoding the last equation, it must be parenthesized as follows in prefix form:

  ap (ap (return (:)) c) (sequence cs)

To make it concrete, let

  c  = putStrLn $ show 1 :: IO ()
  cs = map putStrLn (map show [2, 3]) :: [IO ()]

The type of `ap` is

  ap :: Monad m => m (a -> b) -> m a -> m b

so the interior use of `ap` in equation 1 makes sense: the type of `(:)` is
`a->[a]->[a]`. The type of `return (:)' is `Monad m => m (a -> [a] -> [a])`, or
`IO (a -> [a] -> [a])` in context. This meets the spec of the first argument
of `ap`, and we get

  (ap (return (:))) :: IO a -> IO ([a] -> [a])

Now apply it to an IO () like c:

  (ap (ap (return (:)) (putStrLn $ show 1)) :: IO [()] -> [()]

This entire expression, again, meets the spec of `ap`. We get

Incidentally, even with the definition of c as above, GHCi reports

  (ap (return (:)) c) :: ((a -> [a]) -> a) -> ((a -> [a]) -> [a])

and it's too hard to see how this relates to IO [()] -> [()]. We have to say

  (ap (return (:)) (c :: IO ())) :: IO [()] -> [()]

What is the type of cs?

  cs :: [IO ()]

Therefore,

  sequence cs :: IO [()]

just what `(ap (ap (return (:)) (putStrLn $ show 1))` needs. Finally, we get

  ap (ap (return (:)) (c :: IO ())) (sequence cs) :: IO [()]

In the tests below, we use the Maybe monad for less ambiguity.

-}

applicativeEffects = do
  putStrLn "Applicative Effects"

  let c  =     Just $ show 1
  let cs = map Just (map show [2, 3])

  assertEqual "grotty infix form"      (sequence (c : cs))
                                       (return (:) `ap` c `ap` sequence cs)

  assertEqual "grotty prefix form"     (sequence (c : cs))
                                       (ap (ap (return (:)) c) (sequence cs))

  return ()

--------------------------------------------------------------------------------

main :: IO ()
main = do
  assertEqual "HUnit itself"           42 (6 * 7)

  someTests
  someCompTests
  someCompCompTests
  internalEtaStumbleOn
  applicativeEffects



