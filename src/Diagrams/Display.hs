{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (Text)
import           Lucid

header :: Html ()
header = header_ $ do
  div_ [class_ "navbar navbar-dark bg-dark shadow-sm"] $
    div_ [class_ "container d-flex justify-content-between"] $
      a_ [href_ "#", class_ "navbar-brand d-flex align-items-center"] $
        strong_ "Diagrams backend tests"

bootstrap :: Html ()
bootstrap = link_ [rel_ "stylesheet", crossorigin_ "anonymous", href_ link, integrity_ sha]
  where
    link = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
    sha = "sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO"

liveReload :: Html ()
liveReload = script_ $
  "document.write('<script src=\"http://' + (location.host || 'localhost').split(':')[0] + ':35729/livereload.js?snipver=1\"></' + 'script>')"

basic_path :: [Text]
basic_path =
  [ "square1"
  , "circle1"
  , "circle-square"
  , "2-circles"
  , "ellipse"
  , "arc"
  , "Pre-defined-shapes"
  , "circle-hrule-circle"
  , "poly-example"
  , "star-polygon"
  , "star-skip"
  , "superimposing"
  , "superimposing-color"
  , "juxtaposing1"
  , "juxtaposing2"
  , "line-attributes"
  ]

basic_text :: [Text]
basic_text =
  [ "text-basic"
  , "text-alignment"
  , "text-attributes"
  , "text-transforms"
  , "text-transforms-normal"
  ]

fill_rule :: [Text]
fill_rule =
  [ "ring"
  , "fill-rules"
  , "clip"
  , "clip-multi"
  , "clip-stacked"
  , "alpha-color"
  , "opacity1"
  , "text-opacity"
  , "group-opacity"
  , "fat"
  , "line-loop"
  ]

ref :: Text -> Html ()
ref imgPath = img_ [src_ src, alt_ "ref imgPath", width_ "200", height_ "200"]
  where
    src = "../ref/" <> imgPath <> ".png"

img :: Text -> Html ()
img imgPath = img_ [src_ src, alt_ "svg imgPath", width_ "200", height_ "200"]
  where
    src = "../svg/" <> imgPath <> ".png"

svgRow :: [Text] -> Html ()
svgRow names =
  div_ [class_ "row py-2"] $ do
    flip mapM_ names $ \nm ->
      div_ [class_ "col-lg-6"] $ svgSingle "" nm
      -- div_ [class_ "col-md-2"] $ p_ (toHtml nm)
      -- div_ [class_ "col-md-4"] $ ref nm
      -- div_ [class_ "col-md-4"] $ img nm

svgSingle :: Text -> Text -> Html ()
svgSingle c nm = do
  h5_ (toHtml nm)
  table_ [class_ "table table-hover"] $
    tr_ [class_ c] $ do
    -- tr_ $ do
      td_ $ ref nm
      td_ $ img nm

gridRow :: Html ()
gridRow = do
  -- div_ [class_ "py-3"] $ do

    h2_ "Basic paths"
    svgRow basic_path

    h2_ "Basic text"
    svgRow basic_text

    h2_ "Fill rules"
    svgRow fill_rule

svgTable :: [Text] -> Html ()
svgTable names =
  table_ [class_ "table"] $ do
    -- thead_ [class_ "thead-dark"] $
    --   tr_ $
    --     -- mapM_ (th_ [scope_ "col"]) ["Name", "Reference", "diagrams-svg"]
    --     mapM_ (th_ [scope_ "col"]) ["Reference", "diagrams-svg"]
    tbody_ $
      flip mapM_ names $ \nm ->
        -- tr_ [class_ "table-success"] $ do
        tr_ $ do
          -- th_ [scope_ "row"] $ toHtml nm
          td_ $ ref nm
          td_ $ img nm

tableRow :: Html ()
tableRow = do
  h2_ "Basic paths"
  svgTable basic_path

  h2_ "Basic text"
  svgTable basic_text

  h2_ "Fill rules"
  svgTable fill_rule


-- tableRow :: Html ()
-- tableRow =
--   div_ [class_ "py-3"] $ do

--     h2_ "Lines and loops"

--     table_ [class_ "table table-borderless"] $ do
--       thead_ $ do
--         tr_ $ do
--           mapM_ (th_ [scope_ "col"]) ["Name", "Reference", "diagrams-pgf"]

--       tbody_ $ do
--         tr_ [class_ "table-danger"] $ do
--           th_ [scope_ "row"] $ do
--             p_ "fill-loop"
--             p_ $ small_ "(0.87 err)"
--           td_ $ img "fill-loop"
--           td_ $ img "fill-line"

--         tr_ $ do
--           th_ [scope_ "row"] "line-loop"
--           td_ $ img "clip"
--           td_ $ img "clip"

bod :: Html ()
bod =
  main_ [role_ "main"] $ do
    div_ [class_ "container"] $ do
      -- gridRow
      div_ [class_ "py-3"] $
       div_ [class_ "row"] $ do
         div_ [class_ "col-lg-6"] $ svgTable basic_path
         div_ [class_ "col-lg-6"] $ svgTable basic_text

compact :: Html ()
compact = do
  main_ [role_ "main"] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "py-3"] $ do
        gridRow
        -- h2_ "Fill rules"
        -- div_ [class_ "row"] $ do
        --   div_ [class_ "col-lg-6"] $ svgSingle "table-success" "ring"
        --   div_ [class_ "col-lg-6"] $ svgSingle "table-danger" "line-loop"
        -- -- div_ [class_ "row"] $ do
        --   div_ [class_ "col-lg-6"] $ svgSingle "table-warning" "juxtaposing1"
        --   div_ [class_ "col-lg-6"] $ svgSingle "table-danger" "juxtaposing2"

wrapper :: Html () -> Html ()
wrapper body = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      liveReload
      title_ "Diagrams backend tests"
      bootstrap
    body_ $ do
      header
      body
      -- script_
      --   [src_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"
      --   ,integrity_ "sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy"
      --   ,crossorigin_ "anonymous"
      --   ] ( "" :: Html ())

