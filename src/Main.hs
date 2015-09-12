{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Monad.Trans
import           Data.Function
import           Data.List                     (nubBy)
import           Data.Maybe
import           Data.Monoid                   ((<>))
import           Data.Pool
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Database.PostgreSQL.Simple
import           Graphics.PDF
import           Graphics.PDF.Typesetting
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

-- PDF NONSENSE

data MyVertStyles = NormalPara
                  | CirclePara !PDFFloat

data MyParaStyles = Normal
                  | Bold

instance ComparableStyle MyParaStyles where
  isSameStyleAs Normal Normal = True
  isSameStyleAs Bold Bold = True
  isSameStyleAs _ _ = False

instance Style MyParaStyles where
    textStyle Normal = TextStyle (PDFFont Times_Roman 7) black black FillText 1.0 1.0 1.0 1.0
    textStyle Bold = TextStyle (PDFFont Times_Bold 8) black black FillText 1.0 1.0 1.0 1.0

    sentenceStyle _ = Nothing
    wordStyle _ = Nothing

instance ParagraphStyle MyVertStyles MyParaStyles where
    lineWidth _ w _ = w

instance ComparableStyle MyVertStyles where
    isSameStyleAs NormalPara NormalPara = True
    isSameStyleAs _ _ = False

-- END PDF NONSENSE

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
                                           D.inputHidden "instructions" v
                                           -- D.label "instructions" v "Instructions"
                                           -- D.inputTextArea (Just 20) (Just 50) "instructions" v
                                           -- br
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
                 do -- current numbers amount to 16 full meals
                    let singleDays = 4
                    let doubleDays = 0
                    let tripleDays = 4
                    rs <- liftIO $ getNRecipesWithComplexityGe pg (singleDays + (doubleDays * 2) + (tripleDays * 3)) 3
                    meals <- mapM (\main -> if rComplexity main < 5
                                               then do Just side <- liftIO $ getRecipeWithComplexityLe pg 2
                                                       return [main, side]
                                               else return [main])
                                  rs
                    let singleMeals = Prelude.map (:[]) $ take singleDays meals
                    let doubleMeals = foldUp (take (doubleDays * 2) $ drop singleDays meals)
                    let tripleMeals = foldUp3 (drop (singleDays + (doubleDays * 2)) meals)
                    redirect $ constructPlanUrl (singleMeals <> doubleMeals <> tripleMeals)
               get "/plan" $
                 do ps <- getPlans
                    recipes <- liftIO $ mapM (\day ->
                                                mapM (\meal ->
                                                        mapM (\ri -> do Just r <- getRecipe pg ri
                                                                        is <- getRecipeIngredients pg r
                                                                        return (r, is))
                                                             meal)
                                                     day)
                                             ps
                    let shapes = repeat [Rectangle (5   :+ 205) (295 :+ 395)
                                        ,Rectangle (305 :+ 205) (595 :+ 395)
                                        ,Rectangle (5   :+ 5)   (295 :+ 195)
                                        ,Rectangle (305 :+ 5)   (595 :+ 195)]
                    let pages = zipWith zip (foldUp4 recipes) shapes
                    let rect = PDFRect 0 0 600 400
                    -- NOTE(dbp 2015-09-11): This is the most terribly
                    -- undocumented library I've _ever_ encountered. I
                    -- can't even imagine how stupid you could
                    -- possibly be to release something like this...
                    -- What I've figured out the arguments to rectangle are:
                    -- Rectangle (bottom left X :+ bottom left Y) (top right X :+ top right Y)
                    liftIO $ runPdf "/tmp/mealstrat.pdf" standardDocInfo rect $ do
                      mapM_ (\p -> do page <- addPage Nothing
                                      drawWithPage page $ mapM_ (\(day :: [[(Recipe, [(Ingredient, RecipeIngredient)])]], rect) ->
                                                   displayFormattedText rect NormalPara Normal $
                                                     do paragraph $ do mapM_ (\meal -> do formatMeal (Prelude.map fst meal)
                                                                                          forceNewLine
                                                                                          txt "----"
                                                                                          forceNewLine) day
                                                                  ) p) pages

                    {-blaze $ mapM_ (\day -> do mapM_ (\meal -> formatMeal (Prelude.map fst meal)) day
                                                  formatIngredients (combineIngredients (concat day)))
                                      recipes -}
                    setHeader "Content-Type" "application/pdf"
                    S.file "/tmp/mealstrat.pdf"
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
                      Nothing -> blaze $ do H.style ! type_ "text/css" $ "input { width: 80%; display: inline-block; font-size: 15pt; } textarea { width: 80%; display: inline-block; font-size: 15pt } label { width: 20%; font-size: 15pt; display: inline-block; } select { font-size: 15pt } "
                                            recipeView "/recipes/new" view
               get "/recipes/:id" $
                 do i <- S.param "id"
                    mr <- liftIO $ getRecipe pg i
                    case mr of
                      Nothing -> next
                      Just recipe -> do ingredients <- liftIO $ getRecipeIngredients pg recipe
                                        blaze $ do p $ a ! href "/recipes" $ "All Recipes"
                                                   p (H.text (rName recipe))
                                                   formatIngredients ingredients
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
        formatIngredients = ul . mapM_ (\(i, ri) -> li (H.text (tshow (riQuantity ri) <> " " <> tshow (riUnits ri) <> " " <> iName i)))
        formatMeal (dishes :: [Recipe]) = mapM_ (\recipe -> do txt (T.unpack $ rName recipe)
                                                               forceNewLine) dishes
         -- H.div ! class_ "meal" ! A.style "border: 1px solid #dedede; padding: 10px; margin: 20px; display: inline-block; vertical-align: top" $
         --   do mapM_ (\recipe -> p $ do H.text (rName recipe))
         --            dishes
        foldUp [] = []
        foldUp (x:y:rest) = [x,y] : foldUp rest
        foldUp3 [] = []
        foldUp3 (x:y:z:rest) = [x,y,z] : foldUp3 rest
        foldUp4 [] = []
        foldUp4 (x:y:z:t:rest) = [x,y,z,t] : foldUp4 rest
        combineIngredients :: [(Recipe, [(Ingredient, RecipeIngredient)])] -> [(Ingredient, RecipeIngredient)]
        combineIngredients rs =
          let ingredients = concat $ Prelude.map snd rs in
          let unique = nubBy (\a b -> (iId $ fst a) == (iId $ fst b) && convertable (riUnits (snd a)) (riUnits (snd b))) ingredients in
          Prelude.map (\i ->
                 let others = filter ((== iId (fst i)) . iId . fst) ingredients in
                 let same_units = filter ((== (riUnits (snd i))) . riUnits . snd) others in
                 let diff_units = filter ((/= (riUnits (snd i))) . riUnits . snd) others in
                 let same_count = sum (Prelude.map (riQuantity . snd) same_units) in
                 let diff_count = sum (Prelude.map (\o -> (fromMaybe 0 $ convertUnits (riUnits (snd o)) (riUnits (snd i))) * (riQuantity (snd o))) diff_units)
                 in (fst i, (snd i) { riQuantity = same_count + diff_count } )
              ) unique
