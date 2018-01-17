import Test.HUnit
import Test.QuickCheck
import Lib
import Control.Arrow

-- Really basic stuff to firm up my understanding, proficiency, and fluency.

--------------------------------------------------------------------------------
f :: Num a => a -> a
f x = 2 * x

g :: Num a => a -> a
g x = x ^ 5

someTests = do
  putStrLn    "someTests"
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
  putStrLn    "CompTests"
  assertEqual "nested record access"   0 ( _x ( _location  player1 ) )
  assertEqual "dollar syntax"          0 ( _x $ _location  player1)
  assertEqual "dot syntax"             0 ((_x . _location) player1)

--------------------------------------------------------------------------------

mm = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]]
sqr = \x -> x * x

someCompCompTests = do
  putStrLn    "CompCompTests"
  assertEqual "dotted fmap"            (((fmap . fmap) sqr) mm)
                                       (fmap (\xs -> fmap sqr xs) mm)

  assertEqual "eta inside"             (((fmap . fmap) sqr) mm)
                                       (fmap (fmap sqr) mm)

  assertEqual "replace with dot"       (((fmap . fmap) sqr) mm)
                                       ( (fmap . fmap) sqr  mm)

  assertEqual "prefix notation"        (((fmap . fmap) sqr) mm)
                                       ((.) fmap fmap sqr mm)

  assertEqual "" 0 0

--   (.) :: (b -> c) -> (a -> b) -> a -> c
--
-- Let's apply this to a function of type b -> c. Let's be concrete. Start with
-- a function of type (b = Int) -> (c = String). We want this to be concrete
-- with different types for b and c so that we don't get fooled by unification
-- in the type checker; if the type-checker notices that b and c denote the
-- same type, then it will replace every instance of c with an instance of b in
-- a type expression, making it more difficult for us to trace the calculations
-- of types.
--
--   ((\x -> show x) :: Int -> String) :: Int -> String
--
--   ( (.) ((\x -> show x) :: Int -> String) ) :: (a -> Int) -> a -> String
--
-- This is good: the combination ( (.) ... ) is waiting for an (a -> Int), just
-- as we expected.
--
-- What if we feed (.) to (.)? Let's look at the type of the second (.)
-- expression and map it to (Int -> String) from the first example.
--
--   (.) :: (b -> c) -> (a -> b) -> a -> c
--   ~~~ :: Int      -> String
--
-- If we replace Int with (b -> c) and String with (a' -> b) -> a' -> c, (the
-- two a's are different) we get a type for (.) (.):
--
--   ( (.) (.) ) :: (a -> b -> c) -> a -> (a' -> b) -> a' -> c
--
-- does GHCi agree with us?
--
--   ((.) (.)) :: (a1 -> b -> c) -> a1 -> (a2 -> b) -> a2 -> c
--
-- Yup, with a <- a1 and a' <- a2. This is hard to intuit, so we go back to
-- concrete examples.
--
-- It's waiting for another function. Now, suppose that, instead of feeding
-- (\x -> x + 1) ::
--

--  assertEqual "function as functor"  (((fmap . fmap) sqr) mm)
--                                     ( (.)   . (.)   sqr  mm)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  assertEqual "HUnit itself"           42 (6 * 7)

  someTests
  someCompTests
  someCompCompTests
  internalEtaStumbleOn

