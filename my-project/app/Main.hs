module Main where

import RelationalAlgebra

-- ===========================================================================
-- S A M P L E   D A T A B A S E
-- ===========================================================================

account = Table
     "account"
     [  "acct-num", "branch-name", "balance"]
       --------------------------------------
     [ ["A-101"   , "Downtown"   , "500"    ]
     , ["A-102"   , "Perryridge" , "400"    ]
     , ["A-201"   , "Brighton"   , "900"    ]
     , ["A-215"   , "Mianus"     , "700"    ]
     , ["A-217"   , "Brighton"   , "750"    ]
     , ["A-222"   , "Redwood"    , "700"    ]
     , ["A-305"   , "Round Hill" , "350"    ]
     ]

branch = Table
     "branch"
     [  "branch-name", "branch-city", "assets"  ]
       ------------------------------------------
     [ ["Brighton"   , "Brooklyn"   , "7100000" ]
     , ["Downtown"   , "Brooklyn"   , "9000000" ]
     , ["Mianus"     , "Horseneck"  ,  "400000" ]
     , ["North Town" , "Rye"        , "3700000" ]
     , ["Perryridge" , "Horseneck"  , "1700000" ]
     , ["Pownal"     , "Bennington" ,  "300000" ]
     , ["Redwood"    , "Palo Alto"  , "2100000" ]
     , ["Round Hill" , "Horseneck"  , "8000000" ]
     ]

customer = Table
     "customer"
     [  "cstmr-name", "cstmr-street", "cstmr-city" ]
       ---------------------------------------------
     [ ["Adams"     , "Spring"      , "Pittsfield" ]
     , ["Brooks"    , "Senator"     , "Brooklyn"   ]
     , ["Curry"     , "North"       , "Rye"        ]
     , ["Glenn"     , "Sand Hill"   , "Woodside"   ]
     , ["Green"     , "Walnut"      , "Stamford"   ]
     , ["Hayes"     , "Main"        , "Harrison"   ]
     , ["Johnson"   , "Alma"        , "Palo Alto"  ]
     , ["Jones"     , "Main"        , "Harrison"   ]
     , ["Lindsay"   , "Park"        , "Pittsfield" ]
     , ["Smith"     , "North"       , "Rye"        ]
     , ["Turner"    , "Putnam"      , "Stamford"   ]
     , ["Williams"  , "Nassau"      , "Princeton"  ]
     ]

-- The following is an intersection table.

depositor = Table
     "depositor"
     [  "cstmr-name", "acct-num" ]
       ---------------------------
     [ ["Hayes"     , "A-102"    ]
     , ["Johnson"   , "A-101"    ]
     , ["Johnson"   , "A-201"    ]
     , ["Jones"     , "A-217"    ]
     , ["Lindsay"   , "A-222"    ]
     , ["Smith"     , "A-215"    ]
     , ["Turner"    , "A-305"    ]
     ]

loan = Table
     "loan"
     [  "loan-num", "branch-name", "amount" ]
       --------------------------------------
     [ ["L-11"    , "Round Hill" ,  "900"   ]
     , ["L-14"    , "Downtown"   , "1500"   ]
     , ["L-15"    , "Perryridge" , "1500"   ]
     , ["L-16"    , "Perryridge" , "1300"   ]
     , ["L-17"    , "Downtown"   , "1000"   ]
     , ["L-23"    , "Redwood"    , "2000"   ]
     , ["L-93"    , "Mianus"     ,  "500"   ]
     ]

-- The following is another intersection table.

borrower = Table
     "borrower"
     [  "cstmr-name", "loan-num" ]
       ---------------------------
     [ ["Adams"     , "L-16"     ]
     , ["Curry"     , "L-93"     ]
     , ["Hayes"     , "L-15"     ]
     , ["Jackson"   , "L-14"     ]
     , ["Jones"     , "L-17"     ]
     , ["Smith"     , "L-11"     ]
     , ["Smith"     , "L-23"     ]
     , ["Williams"  , "L-17"     ]
     ]

main :: IO ()
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- MAIN -- REGRESSION

-- The main function of this module is a regression test over all the
-- machinery implemented above. "Regression" means that it exercises a
-- little bit of everything, historically, so we can find out early on
-- whether a new addition breaks something already implemented.

main = do

   printTable (selectf "loan-num" (== "L-16") borrower)

   putStr "Does \"account\" adhere to the given schema?"
   putStr newline
   print (adheresToSchema
      ["acct-num", "branch-name", "balance"] account)

-- Test lookups by various methods.

   print (assocL1 account 0 "acct-num")
   print (oneBasedIndexLookup account 0 1)

-- Hopefully self-explanatory, queries. The suffix "P" on the name of a
-- variable means that it is a predicate. So "perryridgeP" means "is the
-- branch-name 'Perryridge'?"

   perryridgeP <- return (fieldMatchesConstant "branch-name" "Perryridge")
   printTable (select perryridgeP loan) -- page 89 in Silberschatz

   amt1400P <- return (numFieldOpConstP "amount" (>) 1400)
   printTable (select amt1400P loan)

   printTable (select (combinePs perryridgeP (&&) amt1400P) loan)

   tc1   <- return (project ["cstmr-name"] borrower)
   tc2   <- return (project ["cstmr-name"] depositor)
   t3214 <- return (union tc1 tc2)
   printTable t3214

-- Set-difference is '\\'.

   t3215 <- return (tc2 \\ tc1)
   printTable t3215

   perryCustomers <- return (select perryridgeP (cross borrower loan))
   printTable perryCustomers

   loanMatch <- return
     (twoFieldsMatch "borrower.loan-num" "loan.loan-num")
   perryBorrowers <- return
     (project ["cstmr-name"]
       (select loanMatch perryCustomers)) -- page 95
   printTable perryBorrowers

   printTable (rename "Perryridge Borrowers" ["customer-name"]
     perryBorrowers)

   d    <- return (renameTable "d" account)
   ps5  <- return (project ["balance"] account)
   a    <- return (renameTable "a" account)
   ps6  <- return (cross a d)
   ps7  <- return (select
           (numFieldOpNumFieldP "a.balance" (<) "d.balance") ps6)
   ps9  <- return (project ["a.balance"] ps7)
   ps10 <- return (renameSchema ["balance"] ps9)
   ps11 <- return (ps5 \\ ps10)
   printTable ps11

-- Big query on page 97, first some intermediates for debugging

   q1  <- return (selectf "cstmr-name" (== "Smith") customer)
   q2  <- return (project ["cstmr-street", "cstmr-city"] q1)
   q3  <- return (rename "smith-addr" ["street", "city"] q2)
   q4  <- return (cross customer q3)
   qP1 <- return (twoFieldsMatch "cstmr-street" "street")
   qP2 <- return (twoFieldsMatch "cstmr-city"   "city")
   qP  <- return (combinePs qP1 (&&) qP2)
   q5  <- return (select qP q4)
   printTable q5

-- Then the big, ugly, nested expression

   printTable
    (project ["customer.cstmr-name"]
     (select
      (combinePs
       (twoFieldsMatch "customer.cstmr-street" "smith-addr.street") (&&)
       (twoFieldsMatch "customer.cstmr-city"   "smith-addr.city"))
      (cross0 -- critical, here, so as to get overkill renaming
       customer
       (rename "smith-addr" ["street", "city"]
        (project ["cstmr-street", "cstmr-city"]
         (selectf "cstmr-name" (== "Smith")
          customer))))))

   printTable (join borrower loan) -- p. 100

   printTable
    (project ["branch-name"]
     (selectf "cstmr-city" (== "Harrison")
      (join customer (join account depositor))))

   printTable (join customer (join account depositor)) -- fast
   -- printTable (join (join customer account) depositor) -- mucho $low

   printTable (project ["cstmr-name"] (join borrower depositor))

   r1  <- return (project ["branch-name"]
           (selectf "branch-city" (== "Brooklyn") branch))
   r2  <- return (project ["cstmr-name", "branch-name"]
           (join depositor account))

-- The following has a bug:

   -- printTable (r2 `divideBy` r1)

   return ()
