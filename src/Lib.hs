{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Lib where

import Data.Functor.Identity
import           Control.Applicative                  hiding ((<|>))
import           Data.Monoid
import           Data.Pool
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Text.Blaze.Html                      hiding (string)
import qualified Text.Blaze.Html5                     as H
import           Text.Digestive                       hiding (string, choice)
import           Text.Parsec                          hiding (Error)
import           Text.Read (readMaybe)
import Data.Traversable

fieldRead :: Read a => RowParser a
fieldRead = do s <- field
               case readMaybe s of
                 Nothing -> fail ("Could not read value: " <> s)
                 Just v -> return v

data Units = Single | Grams | Ounces | Pounds | Cups | Tablespoons | Teaspoons | CCs deriving (Eq, Show, Read)


data Recipe = Recipe { rId             :: Int
                     , rName           :: Text
                     , rBookId         :: Maybe Int
                     , rPageNumber     :: Maybe Int
                     , rInstructions   :: Text
                     , rTotalTime      :: NominalDiffTime
                     , rActiveTime     :: NominalDiffTime
                     , rNumberServings :: Int
                     , rComplexity     :: Int
                     } deriving (Eq, Show)

instance FromRow Recipe where
  fromRow = Recipe <$> field <*> field <*> field
                   <*> field <*> field <*> (realToFrac <$> (field :: RowParser Double))
                   <*> (realToFrac <$> (field :: RowParser Double)) <*> field <*> field

data Ingredient = Ingredient { iId         :: Int
                             , iName       :: Text
                             , iGramsPerCC :: Maybe Double
                             } deriving (Eq, Show)

instance FromRow Ingredient where
  fromRow = Ingredient <$> field <*> field <*> field

data RecipeIngredient = RecipeIngredient { riRecipeId     :: Int
                                         , riIngredientId :: Int
                                         , riUnits        :: Units
                                         , riQuantity     :: Double
                                         , riOriginalText :: Text
                                         } deriving (Eq, Show)

instance FromRow RecipeIngredient where
  fromRow = RecipeIngredient <$> field <*> field <*> fieldRead
                             <*> field <*> field

data Book = Book { bId     :: Int
                 , bTitle  :: Text
                 , bAuthor :: Text
                 , bYear   :: Int
                 } deriving (Eq, Show)

instance FromRow Book where
  fromRow = Book <$> field <*> field <*> field
                 <*> field

parseResult :: Stream s Identity t => Parsec s () a -> s -> Result Html a
parseResult parser input = case parse parser "" input of
                                   Left err -> Error $ H.text $ T.pack $ show err
                                   Right v -> Success v

parseTime :: Text -> Result Html NominalDiffTime
parseTime = parseResult $ do n <- many1 digit
                             spaces
                             choice [string "m", string "min", string "mins"]
                             return (fromIntegral $ 60 * read n)

parseIngredients :: Text -> Result Html [(Ingredient, RecipeIngredient)]
parseIngredients inp = do let lines = T.lines inp
                          sequenceA (map (\l -> parseResult (ingredientParser l) l) lines)
  where unitParser strings unit = choice (map (try . string) strings) >> return unit
        ingredientParser original =
          do n <- many1 digit
             d <- option 0 $ do char '.'
                                n' <- many1 digit
                                return (read ("0." ++ n'))
             let quantity = read n + d
             spaces
             unit <- option Single (choice [unitParser ["grams", "gram"] Grams
                                           ,unitParser ["ounces","oz","ounce"] Ounces
                                           ,unitParser ["lb", "lbs", "pounds"] Pounds
                                           ,unitParser ["cups", "cup"] Cups
                                           ,unitParser ["tablespoons", "tablespoon", "tbsp", "Tbsp"] Tablespoons
                                           ,unitParser ["teaspoons", "teaspoon", "tsp"] Teaspoons
                                           ,unitParser ["ccs", "cc", "cubic centimeters"] CCs])
             spaces
             desc <- many1 (choice [alphaNum, space])
             return (Ingredient 0 (T.strip $ T.pack desc) Nothing, RecipeIngredient 0 0 unit quantity original)

createIngredients :: Pool Connection -> Int -> [(Ingredient, RecipeIngredient)] -> IO ()
createIngredients pg rid ingredients =
  do mapM_ (\(i, ri) -> do mingredient <- ensureIngredient pg i
                           case mingredient of
                             Nothing -> error "Couldn't create ingredient"
                             Just ingredient -> newRecipeIngredient pg ri { riRecipeId = rid
                                                                          , riIngredientId = iId ingredient})
           ingredients

getRecipes :: Pool Connection -> IO [Recipe]
getRecipes pg = withResource pg (\con -> query_ con "SELECT id, name, book_id, page_number, instructions, total_time, active_time, number_servings, complexity FROM recipes")

getBooks :: Pool Connection -> IO [Book]
getBooks pg = withResource pg (\con -> query_ con "SELECT id, title, author, year FROM books")

newRecipe :: Pool Connection -> Recipe -> IO (Maybe Recipe)
newRecipe pg r@Recipe{..} = withResource pg (\con -> do res <- query con "INSERT INTO recipes (name, book_id, page_number, instructions, total_time, active_time, number_servings, complexity) VALUES (?,?,?,?,?,?,?,?) RETURNING id" (rName, rBookId, rPageNumber, rInstructions, rTotalTime, rActiveTime, rNumberServings, rComplexity)
                                                        case res of
                                                          [Only i] -> return (Just r { rId = i })
                                                          _ -> return Nothing)

newRecipeIngredient :: Pool Connection -> RecipeIngredient -> IO (Maybe RecipeIngredient)
newRecipeIngredient pg ri@RecipeIngredient{..} =
  withResource pg (\con -> do res <- query con "INSERT INTO recipe_ingredients (recipe_id, ingredient_id, units, quantity, original_text) VALUES (?,?,?,?,?) RETURNING true" (riRecipeId, riIngredientId, show riUnits, riQuantity, riOriginalText)
                              case res of
                                [Only True] -> return (Just ri)
                                _ -> return Nothing)

ensureIngredient :: Pool Connection -> Ingredient -> IO (Maybe Ingredient)
ensureIngredient pg i@Ingredient{..} =
  withResource pg (\con -> do res <- query con "SELECT id FROM ingredients where name = ?" (Only iName)
                              case res of
                                [Only id'] -> return (Just i { iId = id' })
                                _ -> do res' <- query con "INSERT INTO ingredients (name, grams_per_cc) VALUES (?,?) RETURNING id" (iName, iGramsPerCC)
                                        case res' of
                                          [Only id''] -> return (Just i { iId = id'' })
                                          _ -> return Nothing
                  )
