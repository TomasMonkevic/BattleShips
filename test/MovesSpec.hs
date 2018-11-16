module MovesSpec where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as LS8

import Moves

deserialisaionTestCase1 :: LS8.ByteString
deserialisaionTestCase1 = LS8.pack "[\"coord\",[\"D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"
        
deserialisaionExpectedResult1 :: Maybe Moves
deserialisaionExpectedResult1 = Just (Moves ("D","2") (Just HIT) (Just (Moves ("H","6") Nothing Nothing)))

deserialisaionTestCase2 :: LS8.ByteString
deserialisaionTestCase2 = LS8.pack "[\"coord\",[\"D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"
        
deserialisaionExpectedResult2 :: Maybe Moves
deserialisaionExpectedResult2 = Just (Moves ("D","2") (Just HIT) (Just (Moves ("H","6") Nothing Nothing)))

movesTests :: Spec
movesTests = do
    describe "Deserialisation tests" $ do
        it "returns moves object from json string" $
            (decode deserialisaionTestCase1 :: Maybe Moves) `shouldBe` deserialisaionExpectedResult1