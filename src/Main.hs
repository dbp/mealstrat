{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Monad.Trans
import           Data.Function
import           Data.List                     (nub, sortBy)
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid                   ((<>))
import           Data.Ord                      (comparing)
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

envDef :: String -> String -> IO String
envDef key def = fromMaybe def <$> lookupEnv key

envDefRead :: Read a => String -> a -> IO a
envDefRead key def = maybe def read <$> lookupEnv key

optparam :: Parsable a => TL.Text -> ActionM (Maybe a)
optparam name = (Just <$> S.param name) `rescue` (\_ -> return Nothing)

recipeForm :: Monad m => [Book] -> Maybe Recipe -> Form Html m (Recipe, [(Ingredient, RecipeIngredient)])
recipeForm books mrecipe =
  (,) <$> (Recipe (fromMaybe 0 (rId <$> mrecipe))
                  <$> "name" .: D.text (rName <$> mrecipe)
                  <*> "book_id" .: choice ((Nothing, "") : Prelude.map (\b -> (Just (bId b), H.text $ bTitle b)) books) (rBookId <$> mrecipe)
                  <*> "page_number" .: optionalStringRead "Must be an integer" (maybe Nothing rPageNumber mrecipe)
                  <*> "instructions" .: D.text (rInstructions <$> mrecipe)
                  <*> "total_time" .: validate parseTime (D.text (formatTime . rTotalTime <$> mrecipe))
                  <*> "active_time" .: validate parseTime (D.text (formatTime . rActiveTime <$> mrecipe))
                  <*> "number_servings" .: stringRead "Must be an integer" (rNumberServings <$> mrecipe)
                  <*> "complexity" .: choice [(1, "1 (Small Side)")
                                             ,(2, "2 (Large Side)")
                                             ,(3, "3 (Normal Dish)")
                                             ,(4, "4 (Large Dish)")
                                             ,(5, "5 (Full Meal)")]
                                             (rComplexity <$> mrecipe))
      <*> "ingredients" .: validate parseIngredients (D.text (maybe Nothing (const (Just "1 Cannot update ingredients yet")) mrecipe))

recipeView :: Text -> View Html -> Html
recipeView action v = D.form v action $
  do D.childErrorList "" v
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
main = do port <- envDefRead "PORT" 3000
          host <- envDef "PGHOST" "localhost"
          pgport <- envDefRead "PGPORT" 5432
          user <- envDef "PGUSER" "mealstrat_user"
          password <- envDef "PGPASS" "111"
          database <- envDef "PGDATABASE" "mealstrat_devel"
          pg <- createPool (connect (ConnectInfo host pgport user password database)) close 1 5 20
          hostname <- envDef "HOSTNAME" "localhost:3000"
          scotty port $
            do get "/static/main.css" $ S.file "static/main.css"
               get "/static/circle.png" $ S.file "static/circle.png"
               get "/" $
                 blaze $ do p $ a ! href "/recipes" $ "All Recipes"
                            H.form ! method "POST" $ do H.label ! for "single" $ "Single Meals"
                                                        H.input ! name "single" ! value "4"
                                                        H.label ! for "double" $ "Double Meals"
                                                        H.input ! name "double" ! value "0"
                                                        H.label ! for "triple" $ "Triple Meals"
                                                        H.input ! name "triple" ! value "4"
                                                        button "Generate Meal Plan"
               post "/" $
                 do -- current numbers amount to 16 full meals
                    singleDays <- S.param "single"
                    doubleDays <- S.param "double"
                    tripleDays <- S.param "triple"
                    rs <- liftIO $ getNRecipesWithComplexityGe pg (singleDays + (doubleDays * 2) + (tripleDays * 3)) 3
                    meals <- mapM (\main -> if rComplexity main == 3
                                               then do Just side <- liftIO $ getRecipeWithComplexityLe pg 2
                                                       return [main, side]
                                               else return [main])
                                  rs
                    let singleMeals = Prelude.map (:[]) $ take singleDays meals
                    let doubleMeals = foldUp (take (doubleDays * 2) $ drop singleDays meals)
                    let tripleMeals = foldUp3 (drop (singleDays + (doubleDays * 2)) meals)
                    let planUrl = constructPlanUrl (singleMeals <> doubleMeals <> tripleMeals)
                    (Just (ShortUrl s _)) <- liftIO $ createShortUrl pg (TL.toStrict planUrl)
                    let target = planUrl <> "&short=" <> TL.fromStrict s
                    liftIO $ updateShortUrl pg s (TL.toStrict target)
                    redirect target
               get "/plan" $
                 do ps <- getPlans
                    recipes <- liftIO $ mapM (\day ->
                                                mapM (\meal ->
                                                        mapM (\ri -> do Just r <- getRecipe pg ri
                                                                        b <- maybe (return Nothing) (getBookById pg) (rBookId r)
                                                                        is <- getRecipeIngredients pg r
                                                                        return ((r, b), is))
                                                             meal)
                                                     day) ps
                    short <- optparam "short"
                    blaze $ do H.body ! onload "alert('This is formatted to print out onto 4-up sheets on Google Chrome 45 for Mac. It may work printing out on other platforms, but has not been tested.')" $
                                 do H.link ! rel "stylesheet" ! href "/static/main.css"
                                    mapM_ (\day -> H.div ! class_ "plan" $
                                                   do let meals = reverse $ zip [1..] day
                                                      let lookupT = M.fromList (concat (Prelude.map (\(n,ms) -> Prelude.map ((,tshow n) . rId . fst . fst) ms) meals))
                                                      let ingredToNs = M.fromListWith (<>) (concat $ Prelude.map (\((r,_), is) -> Prelude.map (\(i,_) -> (iId i, [lookupT M.! (rId r)])) is)
                                                                                                                 (concat day))
                                                      formatIngredients ingredToNs (combineIngredients (concat $ Prelude.map snd (concat day)))
                                                      H.div ! class_ "meals" $ mapM_ (\(n, meal) ->
                                                        formatMeal n (Prelude.map fst meal)) meals
                                                      maybe (return ())
                                                            (\s -> H.div ! A.style "text-align: center;" $ H.text ("http://" <> T.pack hostname <> "/" <> s))
                                                            short)
                                          recipes
               get "/recipes" $
                  do recipes <- liftIO (getRecipes pg)
                     blaze $ do p $ a ! href "/recipes/new" $ "New Recipe"
                                ul $ mapM_ (\r -> li (a ! href (H.textValue $ "/recipes/" <> tshow (rId r)) $ H.text $ rName r)) recipes
               matchAny "/recipes/new" $
                 do books <- liftIO (getBooks pg)
                    (view, result) <- runForm "recipe" (recipeForm books Nothing)
                    case result of
                      Just (recipe, ingredients) -> do mr <- liftIO $ newRecipe pg recipe
                                                       case mr of
                                                         Just r -> liftIO $ createIngredients pg (rId r) ingredients
                                                         Nothing -> error "Couldn't create recipe."
                                                       redirect "/recipes"
                      Nothing -> blaze $ do H.style ! type_ "text/css" $ "input { width: 80%; display: inline-block; font-size: 15pt; } textarea { width: 80%; display: inline-block; font-size: 15pt } label { width: 20%; font-size: 15pt; display: inline-block; } select { font-size: 15pt } "
                                            recipeView "/recipes/new" view
               matchAny "/recipes/:id/edit" $
                 do i <- S.param "id"
                    mr <- liftIO $ getRecipe pg i
                    case mr of
                      Nothing -> next
                      Just recipe -> do books <- liftIO (getBooks pg)
                                        (view, result) <- runForm "recipe" (recipeForm books (Just recipe))
                                        case result of
                                          Just (recipe, _) -> do liftIO $ updateRecipe pg recipe
                                                                 redirect ("/recipes/" <> TL.fromStrict (tshow (rId recipe)))
                                          Nothing -> blaze $ do p $ do a ! href "/recipes" $ "All Recipes"
                                                                       H.text " | "
                                                                       a ! href (H.textValue ("/recipes/" <> tshow (rId recipe))) $ "Back"
                                                                H.style ! type_ "text/css" $ "input { width: 80%; display: inline-block; font-size: 15pt; } textarea { width: 80%; display: inline-block; font-size: 15pt } label { width: 20%; font-size: 15pt; display: inline-block; } select { font-size: 15pt } "
                                                                recipeView ("/recipes/" <> tshow (rId recipe) <> "/edit") view
               get "/recipes/:id" $
                 do i <- S.param "id"
                    mr <- liftIO $ getRecipe pg i
                    case mr of
                      Nothing -> next
                      Just recipe -> do ingredients <- liftIO $ getRecipeIngredients pg recipe
                                        mbook <- liftIO $ maybe (return Nothing) (getBookById pg) (rBookId recipe)
                                        blaze $ do p $ a ! href "/recipes" $ "All Recipes"
                                                   p $ do H.text (rName recipe)
                                                          H.text " "
                                                          a ! href (H.textValue ("/recipes/" <> tshow (rId recipe) <> "/edit")) $ "Edit"
                                                   formatIngredients (M.empty :: M.Map Int [Text]) ingredients
                                                   mapM_ (\l -> p $ H.text l) $ T.lines (rInstructions recipe)
                                                   case mbook of
                                                     Nothing -> return ()
                                                     Just book -> p $ H.text $ maybe "" (("pg" <>) . tshow) (rPageNumber recipe) <> " from " <> bTitle book

               get "/:short" $
                 do short <- S.param "short"
                    mu <- liftIO $ getShortUrl pg short
                    case mu of
                      Nothing -> next
                      Just u -> redirect (TL.fromStrict $ sUrl u)
  where constructPlanUrl plans = TL.fromStrict $
          "/plan?" <> (T.intercalate "&" $ Prelude.map format $ concat . concat $ Prelude.map ((Prelude.map ununTuple) .unTuple) $
            zip [0..] (Prelude.map (zip [0..] . Prelude.map (zip [0..])) plans))
          where unTuple (a,bs) = Prelude.map (a,) bs
                ununTuple (a,(b,cs)) = Prelude.map (\(c,d) -> (a,b,c,d)) cs
                format (a,b,c,d) = "r[" <> tshow a <> "][" <> tshow b <> "][" <> tshow c <> "]=" <> tshow (rId d)
        getPlans = getPlans' [] [] [] 0 0 0
        getPlans' plans day meal p d m =
          (do let k = TL.fromStrict $ "r[" <> tshow p <> "][" <> tshow d <> "][" <> tshow m <> "]"
              cur <- S.param k
              getPlans' plans day (cur : meal) p d (m+1))
          `rescue` (\_ -> if m /= 0
                             then getPlans' plans (meal : day) [] p (d+1) 0
                             else if d /= 0
                                     then getPlans' (day : plans) [] [] (p+1) 0 0
                                     else return plans
                             )
        formatIngredients rt = (ul ! class_ "ingredients") . mapM_ (\(i, ri) -> li $ do H.text ((dropSuffix "." $ dropSuffix ".0" $ T.pack $ take 3 $ show (riQuantity ri)) <> formatUnits (riUnits ri) <> " " <> iName i)
                                                                                        maybe (return ())
                                                                                              (mapM_ (\n -> H.span ! class_ "n" $ H.text n) . reverse . nub)
                                                                                              (M.lookup (iId i) rt))
          where dropSuffix suffix s = fromMaybe s $ T.stripSuffix suffix s
        formatMeal n (dishes :: [(Recipe, Maybe Book)]) =
           H.div ! class_ "meal" $
            do h4 (H.text (tshow n))
               mapM_ (\(recipe, mbook) ->
                        p $ do H.text (rName recipe)
                               H.text $ "; serves " <> tshow (rNumberServings recipe)
                               H.text $ "; " <> formatTime (rTotalTime recipe)
                               maybe (return ()) (\b -> H.text ("; " <> bShort b <> ", p" <> tshow (fromJust (rPageNumber recipe)))) mbook)
                     (reverse $ sortBy (comparing (rComplexity.fst)) dishes)
        foldUp [] = []
        foldUp (x:y:rest) = [x,y] : foldUp rest
        foldUp3 [] = []
        foldUp3 (x:y:z:rest) = [x,y,z] : foldUp3 rest
        foldUp4 [] = []
        foldUp4 (x:y:z:t:rest) = [x,y,z,t] : foldUp4 rest
