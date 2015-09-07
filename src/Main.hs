{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Monad.Trans
import           Data.Function
import           Data.List                     (nubBy)
import           Data.Monoid                   ((<>))
import           Data.Pool
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Database.PostgreSQL.Simple
import           System.Environment
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              hiding (main)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Digestive
import qualified Text.Digestive                as D
import qualified Text.Digestive.Blaze.Html5    as D
import           Text.Digestive.Scotty
import           Web.Scotty
import qualified Web.Scotty                    as S

import           Lib

blaze :: Html -> ActionM ()
blaze = Web.Scotty.html . renderHtml

envDef :: Read a => String -> a -> IO a
envDef key def = maybe def read <$> lookupEnv key

tshow :: Show a => a -> Text
tshow = T.pack . show

optparam :: Parsable a => TL.Text -> ActionM (Maybe a)
optparam name = (Just <$> S.param name) `rescue` (\_ -> return Nothing)

recipeForm :: Monad m => [Book] -> Form Html m (Recipe, [(Ingredient, RecipeIngredient)])
recipeForm books =
  (,) <$> (Recipe 0 <$> "name" .: D.text Nothing
                    <*> "book_id" .: choice ((Nothing, "") : Prelude.map (\b -> (Just (bId b), H.text $ bTitle b)) books) Nothing
                    <*> "page_number" .: optionalStringRead "Must be an integer" Nothing
                    <*> "instructions" .: D.text Nothing
                    <*> "total_time" .: validate parseTime (D.text Nothing)
                    <*> "active_time" .: validate parseTime (D.text Nothing)
                    <*> "number_servings" .: stringRead "Must be an integer" Nothing
                    <*> "complexity" .: choice [(1, "1 (Small Side)")
                                               ,(2, "2 (Large Side)")
                                               ,(3, "3 (Normal Dish)")
                                               ,(4, "4 (Large Dish)")
                                               ,(5, "5 (Full Meal)")]
                                               Nothing)
      <*> "ingredients" .: validate parseIngredients (D.text Nothing)

recipeView :: Text -> View Html -> Html
recipeView action v = D.form v action $ do D.childErrorList "" v
                                           br
                                           D.label "name" v "Name"
                                           D.inputText "name" v
                                           br
                                           D.label "book_id" v "Book"
                                           D.inputSelect "book_id" v
                                           br
                                           D.label "page_number" v "Page Number"
                                           D.inputText "page_number" v
                                           br
                                           D.label "instructions" v "Instructions"
                                           D.inputTextArea (Just 20) (Just 50) "instructions" v
                                           br
                                           D.label "ingredients" v "Ingredients"
                                           D.inputTextArea (Just 20) (Just 50) "ingredients" v
                                           br
                                           D.label "total_time" v "Total Time"
                                           D.inputText "total_time" v
                                           br
                                           D.label "active_time" v "Active Time"
                                           D.inputText "active_time" v
                                           br
                                           D.label "number_servings" v "Number of Servings"
                                           D.inputText "number_servings" v
                                           br
                                           D.label "complexity" v "Complexity"
                                           D.inputSelect "complexity" v
                                           br
                                           D.inputSubmit "Save"

main :: IO ()
main = do port <- envDef "PORT" 3000
          host <- envDef "PGHOST" "localhost"
          pgport <- envDef "PGPORT" 5432
          user <- envDef "PGUSER" "mealstrat_user"
          password <- envDef "PGPASS" "111"
          database <- envDef "PGDATABASE" "mealstrat_devel"
          pg <- createPool (connect (ConnectInfo host pgport user password database)) close 1 5 20
          scotty port $
            do get "/" $
                 do blaze $ do p $ a ! href "/recipes" $ "All Recipes"
                               H.form ! method "POST" $ do button "Get Meal Plan"
               post "/" $
                 do let singleDays = 1
                    let doubleDays = 1
                    rs' <- liftIO $ getNRecipesWithComplexityGe pg (singleDays + (doubleDays * 2)) 3
                    rs <- liftIO $ mapM (\r -> (r, ) <$> getRecipeIngredients pg r) rs'
                    meals <- mapM (\main -> if rComplexity (fst main) < 5
                                               then do Just side <- liftIO $ getRecipeWithComplexityLe pg 2
                                                       ingredients <- liftIO $ getRecipeIngredients pg side
                                                       return [main, (side, ingredients)]
                                               else return [main])
                                  rs
                    let singleMeals = take singleDays meals
                    let doubleMeals = foldUp (drop singleDays meals)
                    blaze $ do h2 "Single"
                               mapM_ (\m -> formatMeal (Prelude.map fst m) >> formatIngredients (combineIngredients m) >> hr) singleMeals
                               h2 "Double"
                               mapM_ (\(m1, m2) -> do formatMeal (Prelude.map fst m1)
                                                      formatMeal (Prelude.map fst m2)
                                                      formatIngredients (combineIngredients $ m1 ++ m2)
                                                      hr)
                                     doubleMeals
               get "/recipes" $
                 do recipes <- liftIO (getRecipes pg)
                    blaze $ do p $ a ! href "/recipes/new" $ "New Recipe"
                               ul $ mapM_ (\r -> li (a ! href (H.textValue $ "/recipes/" <> tshow (rId r)) $ H.text $ rName r)) recipes
               matchAny "/recipes/new" $
                 do books <- liftIO (getBooks pg)
                    (view, result) <- runForm "recipe" (recipeForm books)
                    case result of
                      Just (recipe, ingredients) -> do mr <- liftIO $ newRecipe pg recipe
                                                       case mr of
                                                         Just r -> liftIO $ createIngredients pg (rId r) ingredients
                                                         Nothing -> error "Couldn't create recipe."
                                                       redirect "/recipes"
                      Nothing -> blaze $ do recipeView "/recipes/new" view
               get "/recipes/:id" $
                 do i <- S.param "id"
                    mr <- liftIO $ getRecipe pg i
                    case mr of
                      Nothing -> next
                      Just recipe -> do ingredients <- liftIO $ getRecipeIngredients pg recipe
                                        blaze $ do p $ a ! href "/recipes" $ "All Recipes"
                                                   p (H.text (rName recipe))
                                                   formatIngredients ingredients
  where formatIngredients = ul . mapM_ (\(i, ri) -> li (H.text (tshow (riQuantity ri) <> " " <> tshow (riUnits ri) <> " " <> iName i)))
        formatMeal dishes =
          H.div ! class_ "meal" ! A.style "border: 1px solid #dedede; padding: 10px; margin: 20px; display: inline-block; vertical-align: top" $
            do mapM_ (\recipe -> p $ do H.text (rName recipe))
                     dishes
        foldUp [] = []
        foldUp (x:y:rest) = (x,y) : foldUp rest
        combineIngredients :: [(Recipe, [(Ingredient, RecipeIngredient)])] -> [(Ingredient, RecipeIngredient)]
        combineIngredients rs =
          let ingredients = concat $ Prelude.map snd rs in
          let unique = nubBy ((==) `on` (iId . fst)) ingredients in
          Prelude.map (\i ->
                 let others = filter ((== iId (fst i)) . iId . fst) ingredients in
                 let same_units = filter ((== (riUnits (snd i))) . riUnits . snd) others in
                 let diff_units = filter ((/= (riUnits (snd i))) . riUnits . snd) others in
                 let same_count = sum (Prelude.map (riQuantity . snd) same_units) in
                 let diff_count = sum (Prelude.map (\o -> convertUnits (riUnits (snd o)) (riUnits (snd i)) * (riQuantity (snd o))) diff_units)
                 in (fst i, (snd i) { riQuantity = same_count + diff_count } )
              ) unique
