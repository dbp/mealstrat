{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec

import           Lib

exampleRecipe :: Recipe
exampleRecipe = Recipe 0 "Recipe" (Just 0) (Just 10) "" 0 0 2 3
exampleBook :: Book
exampleBook = Book 0 "Book" "book" "Someone" 2015
exampleIngredient :: Ingredient
exampleIngredient = Ingredient 0 "Ingredient" Nothing
exampleRecipeIngredient :: RecipeIngredient
exampleRecipeIngredient = RecipeIngredient 0 0 Cups 0 "0 cups Ingredient"

main :: IO ()
main =
  hspec $
  do describe "uniqueIngredients" $
       do it "should collapse ingredients with the same id" $
             uniqueIngredients [(exampleIngredient, exampleRecipeIngredient)
                               ,(exampleIngredient, exampleRecipeIngredient)]
               `shouldBe` [(exampleIngredient, exampleRecipeIngredient)]
          it "should not collapse if the units are not convertable" $
            let is = [(exampleIngredient
                      ,exampleRecipeIngredient { riUnits = Single})
                     ,(exampleIngredient
                      ,exampleRecipeIngredient { riUnits = Cups})]  in
            uniqueIngredients is `shouldBe` is
     describe "combineIngredients" $
       do it "should combine two of the same ingredients" $
             combineIngredients [(exampleIngredient
                                 ,exampleRecipeIngredient
                                   { riQuantity = 2})
                                 ,( exampleIngredient
                                  , exampleRecipeIngredient
                                    { riQuantity = 2})
                                  ]
               `shouldBe` [(exampleIngredient
                           ,exampleRecipeIngredient {riQuantity = 4 })]

          it "should combine one ingredient to itself" $
             combineIngredients [(exampleIngredient, exampleRecipeIngredient)]
               `shouldBe` [(exampleIngredient, exampleRecipeIngredient)]
