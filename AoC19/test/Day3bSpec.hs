module Day3bSpec (spec) where

import Test.Hspec
import Day3a (Point)
import Day3b (
  minIntersectSteps,
  eatWireUntil
  )

fn :: [String] -> (Point, Int)
fn = minIntersectSteps . (map (reverse . walkProg . parseProg))

spec = hspec $ do
  describe "my functions" $ do
    it "make sense" $ do
      eatWireUntil 3 [1,2,3,4,5,6] `shouldBe` (3, [4,5,6])
      eatWireUntil 3 [1,2,3] `shouldBe` (3, [])
      eatWireUntil 4 [1,2,3] `shouldBe` (3, [])
  describe "handout examples" $ do
    it "work as shown in the instructions" $ do
      fn ["R8,U5,L5,D3", "U7,R6,D4,L4"] `shouldBe` ((6,5), 30)
      snd . fn ["R75,D30,R83,U83,L12,D49,R71,U7,L72",
                "U62,R66,U55,R34,D71,R55,D58,R83"] `shouldBe` 610
      snd . fn ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"] `shouldBe` 410
