module Lib
  ( someFunc
  )
  where

import Text.Printf
import Text.Read

-- class Has_x r t | r -> t where _x :: Lens r t

-- 'main' is in app/Main.hs. It calls 'someFunc' here.


someFunc :: IO ()
someFunc = do
  putStrLn $ show $ (readMaybe "42" :: Maybe Int)

