Implementation of a relational algebra, following: "Database System
Concepts," by Silberschatz, Korth, and Sudarshan.

===========================================================================

TODO:

1.  Use the "Maybe" monad more ubiquitously, if not comprehensively,
    preferably to the "error" function, for error management.  Start
    off with the unused "columnCount" below.

2.  DONE (was to remove some lameness in "join")

3.  Expand schemata to include domains, represented as Haskell types?
    In the current design, a Schema is just a list of column-header
    names, i.e., attributes without extra domain specifications. Two
    columns have the same domain iff their names match. For more
    richness and better integration with Haskell, apply Haskell
    type-checking to schema validation!

4.  Primary keys are implicit singletons inhabiting the first column
    of every table. Change this: make the primary key a LIST of
    attribute-names, and make it an explicit slot in the Table data
    type.

5.  Nothing distinguishes intersection tables from primary data
    tables.  Fix this.

6.  Figure out how to "gensym" in "fn2", either from the GHC library
    Language.Haskell.THSyntax, or write your own with Monads.  For
    now, there is no Gensym. Thus, the function "fn2" just errors.

7.  Check for duplicates in the schema and in the records of a newly
    constructed table. Not at all sure how to put consistency checks
    in data constructors.

8.  Understand numeric overloading better, for cleaner selection
    predicates.

9.  Understand why 'read "3"' works in the code, but not from the
    console. What is there about the more extensive typing information
    available in the code that makes it work there.

10. Implement positional notation for selection predicates, a`-la page
    98 of Silberschatz. Add the example from p. 97 to the regression
    test in "main".

11. Convert schema and record representations to arrays due to heavy
    use of indexing.

===========================================================================

> module RelationalAlgebra (
>   Table     
>   , project      -- Schema -> Table -> Table
>   , select       -- (Schema -> Record -> Bool) -> Table -> Table 
>   , cross        -- Table -> Table -> Table
>                  --  (renames only colliding field names)
>   , join         -- Table -> Table -> Table
>   , thetaJoin    -- (Schema -> Record -> Bool) -> Table -> Table -> Table
>   , divideBy     -- Table -> Table -> Table
>   , intersect    -- Table -> Table -> Table 
>   , union        -- Table -> Table -> Table 
>   , (\\)         -- Table -> Table -> Table ("set difference")
>   , renameTable  -- Name -> Table -> Table
>   , renameSchema -- Schema -> Table -> Table
>   , rename       -- Name -> Schema -> Table -> Table
>
>   -- some special cases --
>   , selectf      -- String -> (String -> Bool) -> Table -> Table
>   , cross2       -- Table -> Table -> Table
>                  --  (only renames schema in second table
>   , cross0       -- Table -> Table -> Table
>                  --  (renames all fields colliding or not)
>
>   -- some display utilities --
>   , printTable   -- Table -> IO () 
>   , showTable    -- Table -> String
>
>   -- some miscellaneous utilities --
>   , assocL1             -- Table -> Int -> Field -> Maybe Value
>   , assocL2             -- Schema -> Record -> Field -> Maybe Value
>   , oneBasedIndexLookup -- Table -> Int -> Int -> Maybe Value
>   , adheresToSchema     -- Schema -> Table -> Bool
>   , countRecords        -- Table -> Int
>   )
>  where

> import qualified Data.List as List
> import Data.Maybe

Abstract Data Types (ADTs) for a relational table (all data items are
pure strings for simplicity):

> type Name    = String
> type Field   = String
> type Value   = String
> type Schema  = [Field]
> type Record  = [Value]
> type Records = [Record]

> data Table = Table
>               { name    ::  Name       -- table name
>               , schema  ::  Schema     -- field names
>               , records ::  Records }  -- record contents
>   deriving (Eq, Show)

There are some things here we should check, but don't at present
1. length schema == length (records !! 0)
2. schema == (nub schema) -- checks for duplicates
Here would be a check for number 1

> columnCount (Table n s v) = let ls = length s in
>                               if (length v > 0) then
>                                 let rl = length (v!!0) in
>                                   if (rl /= ls) then Nothing
>                                     else Just ls
>                               else Just ls

===========================================================================
S A M P L E   D A T A B A S E 
===========================================================================

> account = Table
>     "account"
>     [  "acct-num", "branch-name", "balance"]
>       --------------------------------------
>     [ ["A-101"   , "Downtown"   , "500"    ]
>     , ["A-102"   , "Perryridge" , "400"    ]
>     , ["A-201"   , "Brighton"   , "900"    ]
>     , ["A-215"   , "Mianus"     , "700"    ]
>     , ["A-217"   , "Brighton"   , "750"    ]
>     , ["A-222"   , "Redwood"    , "700"    ]
>     , ["A-305"   , "Round Hill" , "350"    ]
>     ]

> branch = Table
>     "branch"
>     [  "branch-name", "branch-city", "assets"  ]
>       ------------------------------------------
>     [ ["Brighton"   , "Brooklyn"   , "7100000" ]
>     , ["Downtown"   , "Brooklyn"   , "9000000" ]
>     , ["Mianus"     , "Horseneck"  ,  "400000" ]
>     , ["North Town" , "Rye"        , "3700000" ]
>     , ["Perryridge" , "Horseneck"  , "1700000" ]
>     , ["Pownal"     , "Bennington" ,  "300000" ]
>     , ["Redwood"    , "Palo Alto"  , "2100000" ]
>     , ["Round Hill" , "Horseneck"  , "8000000" ]
>     ]

> customer = Table
>     "customer"
>     [  "cstmr-name", "cstmr-street", "cstmr-city" ]
>       ---------------------------------------------
>     [ ["Adams"     , "Spring"      , "Pittsfield" ]
>     , ["Brooks"    , "Senator"     , "Brooklyn"   ]
>     , ["Curry"     , "North"       , "Rye"        ]
>     , ["Glenn"     , "Sand Hill"   , "Woodside"   ]
>     , ["Green"     , "Walnut"      , "Stamford"   ]
>     , ["Hayes"     , "Main"        , "Harrison"   ]
>     , ["Johnson"   , "Alma"        , "Palo Alto"  ]
>     , ["Jones"     , "Main"        , "Harrison"   ]
>     , ["Lindsay"   , "Park"        , "Pittsfield" ]
>     , ["Smith"     , "North"       , "Rye"        ]
>     , ["Turner"    , "Putnam"      , "Stamford"   ]
>     , ["Williams"  , "Nassau"      , "Princeton"  ]
>     ]

> depositor = Table
>     "depositor"
>     [  "cstmr-name", "acct-num" ]
>       ---------------------------
>     [ ["Hayes"     , "A-102"    ]
>     , ["Johnson"   , "A-101"    ]
>     , ["Johnson"   , "A-201"    ]
>     , ["Jones"     , "A-217"    ]
>     , ["Lindsay"   , "A-222"    ]
>     , ["Smith"     , "A-215"    ]
>     , ["Turner"    , "A-305"    ]
>     ]

> loan = Table
>     "loan"
>     [  "loan-num", "branch-name", "amount" ]
>       --------------------------------------
>     [ ["L-11"    , "Round Hill" ,  "900"   ]
>     , ["L-14"    , "Downtown"   , "1500"   ]
>     , ["L-15"    , "Perryridge" , "1500"   ]
>     , ["L-16"    , "Perryridge" , "1300"   ]
>     , ["L-17"    , "Downtown"   , "1000"   ]
>     , ["L-23"    , "Redwood"    , "2000"   ]
>     , ["L-93"    , "Mianus"     ,  "500"   ]
>     ]

> borrower = Table
>     "borrower"
>     [  "cstmr-name", "loan-num" ]
>       ---------------------------
>     [ ["Adams"     , "L-16"     ]
>     , ["Curry"     , "L-93"     ]
>     , ["Hayes"     , "L-15"     ]
>     , ["Jackson"   , "L-14"     ]
>     , ["Jones"     , "L-17"     ]
>     , ["Smith"     , "L-11"     ]
>     , ["Smith"     , "L-23"     ]
>     , ["Williams"  , "L-17"     ]
>     ]

We need "tuple variables" for associative lookup and 1-based index
lookup. Represent the tuple variable with a zero-based index of the
tuple in the relation instead of with a putative tuple value to avoid
the necessity of membership checking, which is linear-time in the size
of the table.

> assocL1 :: Table -> Int -> Field -> Maybe Value
> assocL1 (Table n s v) whichTupleZeroBased field =
>  let tupleVar = v !! whichTupleZeroBased
>  in  assocL2 s tupleVar field

> assocL2 :: Schema -> Record -> Field -> Maybe Value
> assocL2 schema record field = 
>  lookup field (zip schema record)

In the next one, we can be more clever with the "Maybe" monad:

> oneBasedIndexLookup :: Table -> Int -> Int -> Maybe Value
> oneBasedIndexLookup (Table n s v) whichTupleZeroBased iField =
>  if ((iField <= (length s)) && (iField >= 1)) then
>   Just ((v !! whichTupleZeroBased) !! (iField-1))
>  else
>   Nothing

To test whether a relation (a table) adheres to a given schema, s:

> adheresToSchema :: Schema -> Table -> Bool
> adheresToSchema schema (Table n s v) =
>  schema == s

> countRecords :: Table -> Int
> countRecords (Table n s v) = length v

===========================================================================
Pretty Print
===========================================================================

> maxWidths t@(Table n s v) =
>  maxWidths' ((fieldNameWidths t) : (columnWidths t)) where
>    maxWidths' :: [[Int]] -> [Int]
>    maxWidths' []        = []
>    maxWidths' [una]     = una -- if just one list, then it's max
>    maxWidths' (una:mas) = zipWith max una (maxWidths' mas)
>    columnWidths (Table n s v) = map (map length) v
>    fieldNameWidths (Table n s v) = map length s
                           
> showTable t@(Table n s v) =
>   let ms = maxWidths t
>       ss = sum ms + 1 + length ms
>       br = nChars '-' ss ++ newline
>       cd = length v
>   in newline
>     ++ "[" ++ show cd ++ "]:" ++ n ++ newline
>     ++ br
>     ++ formedLine ms s ++ newline
>     ++ br
>     ++ concat (map (\s -> formedLine ms s ++ newline) v)
>     ++ br
>     where
>      formedLine lengs stufs =
>        "|" ++ (concat 
>        (zipWith (\l s -> capped (strRtPaddedTo l s)) lengs stufs))
>
>      strRtPaddedTo n str = let ln = length str in
>        if (n >= ln) then str ++ nSpaces (n - ln)
>        else take n str
>
>      nSpaces = nChars ' '
>
>      nChars :: Char -> Int -> [Char]
>      nChars = flip replicate
>
>      capped str = str ++ "|"

> newline = ['\r', '\n']

> printTable = putStr . showTable

===========================================================================
Projection (Selecting Columns)
===========================================================================

Find the presumed first and only match for a given string in a list of
strings.  We frequently partially apply this to a list to get a mappable
function of a str.

> ixOfStr lyst str = ixs' 0 lyst str where
>   ixs' i [] s     = -1
>   ixs' i (z:zs) s | (s == z)  = i
>                   | otherwise = ixs' (i+1) zs s

The following permits reordering of columns when searching for
indices. For instance, ixsOfColumns ["Nr", "Clip"] -> [0,2], and
ixsOfColumns ["Clip", "Nr"] -> [2,0]

> nonNeg = (>= (0::Int))

> ixsOfColumns cols schema = filter nonNeg (map (ixOfStr schema) cols)
>
> pickElementsByIxs iocs lyst = map (lyst !!) iocs

In the following, "nub" makes sure there are no duplicate
rows in the result.

> project :: Schema -> Table -> Table
> project cols (Table nym schema values) =
>   let is = ixsOfColumns cols schema in
>     Table ("proj (" ++ nym ++ ")")
>           (pickElementsByIxs is schema)
>           (List.nub (map (pickElementsByIxs is) values))

===========================================================================
Selecting Records
===========================================================================

> selectf :: Field -> (Value -> Bool) -> Table -> Table
> selectf field pred (Table n s v) =
>   let i  = ixOfStr s field
>       nn = ("selectf (" ++ n ++ ")") in
>     if (i == (-1)) then (Table nn [] [[]])
>     else Table nn s (filter (\z -> (pred (z !! i))) v)

Here is a more general "select" function. First, note that the
predicate accesses the schema, and the predicates will be more
complicated in this general setting. We partially apply the predicate
to the schema, resulting in a function of a record, and we just filter
that over the input table's record values.

> select :: (Schema -> Record -> Bool) -> Table -> Table
> select pred t@(Table n s v) =
>   let nn = ("select (" ++ n ++ ")") in
>     Table nn s (filter (pred s) v)

First argument to this better "select" is a function from a schema and
a record to a bool. Here are some predicate-construction helpers:

> fieldOpConstP field ordop const =
>  (\schema record ->
>   (fromJust
>    (assocL2 schema record field)) `ordop` const)

> fieldMatchesConstant field value = 
>  fieldOpConstP field (==) value

> numFieldOpConstP field ordop const =
>  (\schema record ->
>   (read
>    (fromJust
>     (assocL2 schema record field))) `ordop` const)

> fieldOpFieldP f1 op f2 = 
>  (\schema record ->
>    assocL2 schema record f1 `op`
>    assocL2 schema record f2)

> twoFieldsMatch f1 f2 =
>  fieldOpFieldP f1 (==) f2

> numFieldOpNumFieldP f1 op f2 =
>  (\schema record ->
>    let v1 = (read (fromJust (assocL2 schema record f1)))
>        v2 = (read (fromJust (assocL2 schema record f2)))
>    in -- TODO: fix this kludge to force selection of numeric overload.
>       --       Vapid boolean expressions here. I don't know a cleaner
>       --       way to force selection of the numeric overload.
>        ((v1 > 0) || (v1 <= 0)) && -- take these two lines out to 
>        ((v2 > 0) || (v2 <= 0)) && -- see the generated error
>        v1 `op` v2)

> combinePs p1 combop p2 = 
>  (\schema record ->
>    ((p1 schema record) `combop` (p2 schema record)))

===========================================================================
The rest of the operators
===========================================================================

Error-checking gadget:

> assert bool failstring = if (not bool) then error failstring else True

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

> union (Table n1 s1 v1) (Table n2 s2 v2) = 
>  let x = assert (s1 == s2) "Schema mismatch" in x `seq` -- force eval
>   Table ("union (" ++ n1 ++ ", " ++ n2 ++ ")") 
>    s1 (List.union v1 v2)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

> intersect (Table n1 s1 v1) (Table n2 s2 v2) = 
>  let x = assert (s1 == s2) "Schema mismatch" in x `seq` -- force eval
>   Table ("intersect (" ++ n1 ++ ", " ++ n2 ++ ")")
>    s1 (List.intersect v1 v2)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The set-difference operator \\ of section 3.2.1.5., modeled after the
similar one in standard module "List."

> (\\) (Table n1 s1 v1) (Table n2 s2 v2) = 
>  let x = assert (s1 == s2) "Schema mismatch" in x `seq` -- force eval
>   Table ("diff (" ++ n1 ++ ", " ++ n2 ++ ")")
>    s1 (v1 List.\\ v2)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

> renameTable :: Name -> Table -> Table
> renameTable newName (Table n s v) =
>  Table newName s v

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

> renameSchema :: Schema -> Table -> Table
> renameSchema newSchema (Table n s v) =
>  let x = assert ((length s) == (length newSchema)) "Length mismatch"
>  in x `seq` Table n newSchema v -- "seq" forces evaluation of assert

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

> rename :: Name -> Schema -> Table -> Table
> rename n s t = 
>  renameTable n (renameSchema s t)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cartesian Product

  Preliminaries

> dot s t = (s ++ "." ++ t)

The following version of cross renames both table schemata by 
prefixing the table name to each attribute name:

> cross0 (Table n1 s1 v1) (Table n2 s2 v2) =
>  let nf2 = fn2 n1 n2
>      sf1 = map (dot n1) s1
>      sf2 = map (dot nf2) s2
>      ncl = cross' v1 v2
>      x   = assert ((List.nub ncl) == ncl) "Unexpected duplicates"
>  in x `seq` -- forces "assert" to execute
>      Table ("cross0 (" ++ n1 ++ ", " ++ nf2 ++ ")")
>            (sf1 ++ sf2)
>            ncl

  The next two versions are smarter: they rename only colliding
  field names, either in both schemata or in just one of the two,
  the second, chosen arbitrarily.

  Fixing collisions on attributes, "fanc" stands for "Fix
  Attribute-Name Collision," "ftnc" stands for "Fix Table-Name
  Collision."

  Desirable precondition: the schema should have no duplicates. This
  function just fixes the first collision and stops. Duplicates will
  be ignored.

> mapWhen p f = map (\z -> if p z then f z else z)
    
> fanc tblnym attrs a = mapWhen (a ==) (dot tblnym) attrs

  The following does a "flatmap" of (fanc tn aLs) on aRs, that is,
  'replaces' aLs with (fanc tn aLs aR) for each aR in aRs, fixing name
  collisions in the interior argument.

> fancs _  aLs []       = aLs
> fancs tn aLs (aR:aRs) = fancs tn (fanc tn aLs aR) aRs

  The following fixes the table name of the second table only, just
  arbitrarily chosen. "Gensym" should eventually go here; for now, 
  it errors. Once it's fixed up with gensym, its calls will not have
  to change.

> fn2 n1 n2 =
>  if (n1 == n2) then error "Rename tables first!" else n2

> cross' al bl = concat (map (\x -> (map (x ++) bl)) al)

Here is a version that renames both the left and right-hand schemas,
but fixes the table name only on the right-hand table.

> cross (Table n1 s1 v1) (Table n2 s2 v2) =
>  let nf2 = fn2 n1 n2
>      sf1 = fancs n1  s1 s2
>      sf2 = fancs nf2 s2 s1
>      ncl = cross' v1 v2
>      x   = assert ((List.nub ncl) == ncl) "Unexpected duplicates"
>  in x `seq` -- forces "assert" to execute
>      Table ("cross (" ++ n1 ++ ", " ++ nf2 ++ ")")
>            (sf1 ++ sf2)
>            ncl

Here is a version that renames only the right-hand schema, chosen
arbitrarily, but resulting in a shorter printout. This form will
be crucial for a decent implementation of "join".

> cross2 (Table n1 s1 v1) (Table n2 s2 v2) =
>  let nf2 = fn2 n1 n2
>      sf2 = fancs nf2 s2 s1
>      ncl = cross' v1 v2
>      x   = assert ((List.nub ncl) == ncl) "Unexpected duplicates"
>  in x `seq` -- forces "assert" to execute
>      Table ("cross2 (" ++ n1 ++ ", " ++ nf2 ++ ")")
>            (s1 ++ sf2)
>            ncl

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Natural Join

The formal spec for t1 `join` t2 is
 (project (s1 `List.union` s2)
          (select (t1.A1 == t2.A2 ... t1.An == t2.An)
          (cross t1 t2)))

where s1 = schema t1, s2 = schema t2 and {A1, ..., An} == (s1
`List.intersect` s2). But we must do a few little things to make this
real. First of all, cross renames things via fancs. We need something
like that for join, but it must report the fixed names. Furthermore,
so the List.union will get ONE of each of the matching columns, we
must only patch the right-hand one, s2, the same way cross2 does.

  Precondition: n1 != n2

> dropUntil p []     =  []
> dropUntil p (c:cs) | (p c)     = cs
>                    | otherwise = dropUntil p cs

> unDot s = dropUntil (== '.') s

> fixedNames n1 s1 n2 s2 =
>  let sf1 = fancs n1 s1 s2
>      sf2 = fancs n2 s2 s1
>      x   = assert (n1 /= n2) "Tables must have different names here !"
>  in x `seq` -- forces "assert" to execute
>      let a = (sf1 List.\\ s1)
>          b = (sf2 List.\\ s2)
>      in if (not (null (a `List.intersect` b))) then
>            error "Catastrophic failure: fixedNames"
>          else (map unDot a,b) -- unfix left-hand names so "union"
>                               -- will pick it up.

Construct a predicate preimage for testing that equality obtains on the
"fixedNames". To make a predicat, apply this to a "fixedNames":

> joinPred (s1,s2) schema record =
>  let s1picks = map fromJust (map (assocL2 schema record) s1)
>      s2picks = map fromJust (map (assocL2 schema record) s2)
>      bools   = zipWith (==) s1picks s2picks
>      final   = foldl (&&) True bools
>  in  final

Take a deep breath:

> join t1@(Table n1 s1 v1) t2@(Table n2 s2 v2) =
>  let nf2 = fn2 n1 n2
>      fns = fixedNames n1 s1 nf2 s2
>      t3  = cross2 t1 t2 -- crucial it be cross2 here
>      t4  = select (joinPred fns) t3
>      rus = s1 `List.union` s2
>      pj  = project rus t4
>  in  renameTable ("join (" ++ n1 ++ ", " ++ nf2 ++ ")") pj

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
From p. 101

> thetaJoin :: (Schema -> Record -> Bool) -> Table -> Table -> Table
> thetaJoin p t1 t2 = (select p (join t1 t2))

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Division

  First, a (stupid) quadratic subset check

> [] `subset` [] = True
> s1 `subset` s2 = and (map (\f -> f s2) (map elem s1))

> divideBy r@(Table n1 s1 v1) s@(Table n2 s2 v2) =
>  let x   = assert (s1 `subset` s2) "Divide precondition S subset R"
>      rms = s1 List.\\ s2
>      t1  = project (rms ++ s2) r
>      t2  = cross (project rms r) s
>      t3  = project rms (t2 \\ t1)
>      t4  = project rms r
>  in  x `seq` 
>      renameTable
>      ("divideBy (" ++ n1 ++ ", " ++ n2 ++ ")")
>      (t4 \\ t3)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MAIN -- REGRESSION

The main function of this module is a regression test over all the 
machinery implemented above. "Regression" means that it exercises a
little bit of everything, historically, so we can find out early on
whether a new addition breaks something already implemented.

> main = do

>   printTable (selectf "loan-num" (== "L-16") borrower)

>   putStr "Does \"account\" adhere to the given schema?"
>   putStr newline
>   print (adheresToSchema
>      ["acct-num", "branch-name", "balance"] account)

Test lookups by various methods.

>   print (assocL1 account 0 "acct-num")
>   print (oneBasedIndexLookup account 0 1)

Hopefully self-explanatory, queries. The suffix "P" on the name of a
variable means that it is a predicate. So "perryridgeP" means "is the
branch-name 'Perryridge'?"

>   perryridgeP <- return (fieldMatchesConstant "branch-name" "Perryridge")
>   printTable (select perryridgeP loan) -- page 89 in Silberschatz

>   amt1400P <- return (numFieldOpConstP "amount" (>) 1400)
>   printTable (select amt1400P loan)

>   printTable (select (combinePs perryridgeP (&&) amt1400P) loan)

>   tc1   <- return (project ["cstmr-name"] borrower)
>   tc2   <- return (project ["cstmr-name"] depositor)
>   t3214 <- return (union tc1 tc2)
>   printTable t3214

Set-difference is '\\'.

>   t3215 <- return (tc2 \\ tc1)
>   printTable t3215

>   perryCustomers <- return (select perryridgeP (cross borrower loan))
>   printTable perryCustomers

>   loanMatch <- return
>     (twoFieldsMatch "borrower.loan-num" "loan.loan-num")
>   perryBorrowers <- return
>     (project ["cstmr-name"]
>       (select loanMatch perryCustomers)) -- page 95
>   printTable perryBorrowers

>   printTable (rename "Perryridge Borrowers" ["customer-name"]
>     perryBorrowers)

>   d    <- return (renameTable "d" account)
>   ps5  <- return (project ["balance"] account)
>   a    <- return (renameTable "a" account)
>   ps6  <- return (cross a d)
>   ps7  <- return (select
>           (numFieldOpNumFieldP "a.balance" (<) "d.balance") ps6)
>   ps9  <- return (project ["a.balance"] ps7)
>   ps10 <- return (renameSchema ["balance"] ps9)
>   ps11 <- return (ps5 \\ ps10)
>   printTable ps11

Big query on page 97, first some intermediates for debugging

>   q1  <- return (selectf "cstmr-name" (== "Smith") customer)  
>   q2  <- return (project ["cstmr-street", "cstmr-city"] q1)
>   q3  <- return (rename "smith-addr" ["street", "city"] q2)
>   q4  <- return (cross customer q3)
>   qP1 <- return (twoFieldsMatch "cstmr-street" "street")
>   qP2 <- return (twoFieldsMatch "cstmr-city"   "city")
>   qP  <- return (combinePs qP1 (&&) qP2) 
>   q5  <- return (select qP q4)
>   printTable q5

Then the big, ugly, nested expression

>   printTable    
>    (project ["customer.cstmr-name"] 
>     (select
>      (combinePs
>       (twoFieldsMatch "customer.cstmr-street" "smith-addr.street") (&&)
>       (twoFieldsMatch "customer.cstmr-city"   "smith-addr.city"))
>      (cross0 -- critical, here, so as to get overkill renaming
>       customer
>       (rename "smith-addr" ["street", "city"]
>        (project ["cstmr-street", "cstmr-city"]
>         (selectf "cstmr-name" (== "Smith")
>          customer))))))

>   printTable (join borrower loan) -- p. 100

>   printTable
>    (project ["branch-name"]
>     (selectf "cstmr-city" (== "Harrison")
>      (join customer (join account depositor))))

>   printTable (join customer (join account depositor)) -- fast
>   -- printTable (join (join customer account) depositor) -- mucho $low

>   printTable (project ["cstmr-name"] (join borrower depositor))

>   r1  <- return (project ["branch-name"]
>           (selectf "branch-city" (== "Brooklyn") branch))
>   r2  <- return (project ["cstmr-name", "branch-name"]
>           (join depositor account))
>      
>   printTable (r2 `divideBy` r1)

 