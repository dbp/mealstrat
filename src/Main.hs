{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans
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
                 do undefined
                    undefined
               get "/recipes" $
                 do recipes <- liftIO (getRecipes pg)
                    blaze $ do p $ a ! href "/recipes/new" $ "New Recipe"
                               ul $ mapM_ (\r -> li (H.text $ rName r)) recipes
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
                 do -- i <- S.param "id"
                    undefined
