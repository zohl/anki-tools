{-# LANGUAGE LambdaCase #-}

module MainTest where

import Data.Default (def)
import System.Environment (getArgs)
import Anki.Tools
import Anki.Collection
-- import Anki.Note
-- import Anki.Card
import Anki.UserProfile

import Test.Hspec

spec_getUserProfile :: Spec
spec_getUserProfile =
  describe "getUserProfile" $
    it "somehow handles Nothing" $ do
      userProfile <- getUserProfile Nothing
      userProfile `shouldBe` UserProfile "default"

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
  _collection <- getCollection userProfile $ read <$> (lookup "--collection" args)
  _notes <- getNotes def userProfile
  cards <- getCards def userProfile

  putStrLn (show cards)
