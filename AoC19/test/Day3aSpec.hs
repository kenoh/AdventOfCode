module Day3aSpec (spec) where

import Test.Hspec
import Day3 (
  findShortestDistance, 
  findIntersections, 
  walkProg,
  parseProg,
  walkDirection
  )

fn = findShortestDistance . findIntersections . (map walkProg) . (map parseProg)

spec = hspec $ do
  describe "my functions" $ do
    it "make sense" $ do
      walkDirection (1,2) 'U' 3 `shouldBe` [(1,3),(1,4),(1,5)]
      walkProg [('L',2)] `shouldBe` [(-2,0),(-1,0),(0,0)]
      walkProg [('L',2),('U',4),('D',7)] `shouldBe` [(-2,-3),(-2,-2),(-2,-1),(-2,0),(-2,1),(-2,2),(-2,3),(-2,4),(-2,3),(-2,2),(-2,1),(-2,0),(-1,0),(0,0)]
      --findIntersections [(0,0)] `shouldBe` []
  describe "handout examples" $ do
    it "work as shown in the instructions" $ do
      fn ["R8,U5,L5,D3", "U7,R6,D4,L4"] `shouldBe` 6
