{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Lib where

import Data.List (nubBy)
import Control.Monad (void)
import Data.Maybe
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
import Data.Maybe

tshow :: Show a => a -> Text
tshow = T.pack . show

fieldRead :: Read a => RowParser a
fieldRead = do s <- field
               case readMaybe s of
                 Nothing -> fail ("Could not read value: " <> s)
                 Just v -> return v

data Units = Single | Grams | Ounces | Pounds | Cups | Tablespoons | Teaspoons | CCs deriving (Eq, Show, Read)

formatUnits :: Units -> Text
formatUnits Single = ""
formatUnits Grams = "gm"
formatUnits Ounces = "oz"
formatUnits Pounds = "lb"
formatUnits Cups = " cups"
formatUnits Tablespoons = " Tbsp"
formatUnits Teaspoons = " tsp"
formatUnits CCs = "cc"

formatTime :: NominalDiffTime -> Text
formatTime t = tshow (floor $ 1/60 * (realToFrac t)) <> "mins"

convertable :: Units -> Units -> Bool
convertable from to = isJust (convertUnits from to)

convertUnits :: Units -> Units -> Maybe Double
convertUnits Tablespoons Teaspoons = Just 3
convertUnits Teaspoons Tablespoons = Just (1/3)
convertUnits Cups Tablespoons = Just 16
convertUnits Tablespoons Cups = Just (1/16)
convertUnits from to = if from == to then Just 1 else Nothing

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
                 , bShort  :: Text
                 , bAuthor :: Text
                 , bYear   :: Int
                 } deriving (Eq, Show)

instance FromRow Book where
  fromRow = Book <$> field <*> field <*> field
                 <*> field <*> field


data ShortUrl = ShortUrl { sShort     :: Text
                         , sUrl  :: Text
                         } deriving (Eq, Show)

instance FromRow ShortUrl where
  fromRow = ShortUrl <$> field <*> field

parseResult :: Stream s Identity t => Parsec s () a -> s -> Result Html a
parseResult parser input =
  case parse parser "" input of
          Left err -> Error $ H.text $ T.pack $ show err
          Right v -> Success v

parseTime :: Text -> Result Html NominalDiffTime
parseTime = parseResult $ do n <- many1 digit
                             spaces
                             choice [string "m", string "min", string "mins"]
                             return (fromIntegral $ 60 * read n)

unitParser strings unit = choice (map (try . string) strings) >> return unit
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

parseIngredients :: Text -> Result Html [(Ingredient, RecipeIngredient)]
parseIngredients inp = do let lines = T.lines inp
                          sequenceA (map (\l -> parseResult (ingredientParser l) l) lines)

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

getNRecipesWithComplexityGe :: Pool Connection -> Int -> Int -> IO [Recipe]
getNRecipesWithComplexityGe pg num comp = withResource pg (\con -> query con "SELECT id, name, book_id, page_number, instructions, total_time, active_time, number_servings, complexity FROM recipes WHERE complexity >= ? ORDER BY random() LIMIT ?" (comp, num))

getRecipeWithComplexityLe :: Pool Connection -> Int -> IO (Maybe Recipe)
getRecipeWithComplexityLe pg comp = withResource pg (\con -> listToMaybe <$> query con "SELECT id, name, book_id, page_number, instructions, total_time, active_time, number_servings, complexity FROM recipes WHERE complexity <= ? ORDER BY random() LIMIT 1" (Only comp))

getRecipe :: Pool Connection -> Int -> IO (Maybe Recipe)
getRecipe pg i = withResource pg (\con -> listToMaybe <$> query con "SELECT id, name, book_id, page_number, instructions, total_time, active_time, number_servings, complexity FROM recipes WHERE id = ?" (Only i))

deleteRecipe :: Pool Connection -> Int -> IO ()
deleteRecipe pg i = withResource pg (\con -> do execute con "DELETE FROM recipe_ingredients WHERE recipe_id = ?" (Only i)
                                                execute con "DELETE FROM recipes WHERE id = ?" (Only i)
                                                return ())

getBooks :: Pool Connection -> IO [Book]
getBooks pg = withResource pg (\con -> query_ con "SELECT id, title, short, author, year FROM books")

getBookById :: Pool Connection -> Int -> IO (Maybe Book)
getBookById pg i = withResource pg (\con -> listToMaybe <$> query con "SELECT id, title, short, author, year FROM books WHERE id = ?" (Only i))

createShortUrl :: Pool Connection -> Text -> IO (Maybe ShortUrl)
createShortUrl pg target = withResource pg (\con -> listToMaybe <$> query con "INSERT INTO short_urls (short, url) VALUES (substr(md5(random()::text), 0, 6),?) RETURNING short, url" (Only target))

updateShortUrl :: Pool Connection -> Text -> Text -> IO ()
updateShortUrl pg short target = withResource pg (\con -> void $ execute con "UPDATE short_urls SET url = ? WHERE short = ?" (target, short))

getShortUrl :: Pool Connection -> Text -> IO (Maybe ShortUrl)
getShortUrl pg s = withResource pg (\con -> listToMaybe <$> query con "SELECT short, url FROM short_urls WHERE short = ?" (Only s))

getRecipeIngredients :: Pool Connection -> Recipe -> IO [(Ingredient, RecipeIngredient)]
getRecipeIngredients pg recipe = withResource pg (\con -> do res <- query con "SELECT I.id, I.name, I.grams_per_cc, R.recipe_id, R.ingredient_id, R.units, R.quantity, R.original_text FROM ingredients AS I JOIN recipe_ingredients as R on R.ingredient_id = I.id WHERE R.recipe_id = ?" (Only (rId recipe))
                                                             return $ map (\(i :. ri) -> (i, ri)) res)

newRecipe :: Pool Connection -> Recipe -> IO (Maybe Recipe)
newRecipe pg r@Recipe{..} = withResource pg (\con -> do res <- query con "INSERT INTO recipes (name, book_id, page_number, instructions, total_time, active_time, number_servings, complexity) VALUES (?,?,?,?,?,?,?,?) RETURNING id" (rName, rBookId, rPageNumber, rInstructions, rTotalTime, rActiveTime, rNumberServings, rComplexity)
                                                        case res of
                                                          [Only i] -> return (Just r { rId = i })
                                                          _ -> return Nothing)

updateRecipe :: Pool Connection -> Recipe -> IO ()
updateRecipe pg Recipe{..} = withResource pg (\con -> do void $ execute con "UPDATE recipes SET name = ?, book_id = ?, page_number = ?, instructions = ?, total_time = ?, active_time = ?, number_servings = ?, complexity = ? WHERE id = ?" (rName, rBookId, rPageNumber, rInstructions, rTotalTime, rActiveTime, rNumberServings, rComplexity, rId))

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


uniqueIngredients :: [(Ingredient, RecipeIngredient)] -> [(Ingredient, RecipeIngredient)]
uniqueIngredients = nubBy (\a b -> (iId $ fst a) == (iId $ fst b) && convertable (riUnits (snd a)) (riUnits (snd b)))

combineIngredients :: [(Ingredient, RecipeIngredient)] -> [(Ingredient, RecipeIngredient)]
combineIngredients ingredients =
  Prelude.map (\i ->
    let others = filter ((== iId (fst i)) . iId . fst) ingredients in
    let same_units = filter ((== (riUnits (snd i))) . riUnits . snd) others in
    let diff_units = filter ((/= (riUnits (snd i))) . riUnits . snd) others in
    let same_count = sum (Prelude.map (riQuantity . snd) same_units) in
    let diff_count = sum (Prelude.map (\o -> (fromMaybe 0 $ convertUnits (riUnits (snd o)) (riUnits (snd i))) * (riQuantity (snd o))) diff_units)
    in (fst i, (snd i) { riQuantity = same_count + diff_count } )
    ) (uniqueIngredients ingredients)
