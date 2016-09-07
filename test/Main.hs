{-# LANGUAGE LambdaCase #-}

import Data.Default (def)
import System.Environment (getArgs)
import Anki.Tools
import Anki.Types
import Anki.Misc


splitArgs :: [String] -> [(String, String)]
splitArgs []       = []
splitArgs (_:[])   = error "splitArgs failed to process args"
splitArgs (x:y:xs) = (x, y):(splitArgs xs)

firstMatch :: (Eq a) => (b -> a) -> Maybe a -> ([b] -> b)
firstMatch f = maybe head (\x -> head . filter ((== x) . f))

getUserProfile :: Maybe String -> IO UserProfile
getUserProfile m = firstMatch upName m <$> getUserProfiles def

getCollection :: UserProfile -> Maybe Int -> IO Collection
getCollection p m = firstMatch collectionId m <$> getCollections def p

main :: IO ()
main = do
  args <- splitArgs <$> getArgs

  userProfile <- getUserProfile $ lookup "--user-profile" args
  -- let output = show userProfile

  -- collection <- getCollection userProfile $ read <$> (lookup "--collection" args)
  -- let output = show collection

  -- notes <- getNotes def userProfile
  -- let output = show notes

  cards <- getCards def userProfile
  let output = show cards

  putStrLn output
