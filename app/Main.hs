module Main where

import Lib

import Control.Monad
import Control.Concurrent (threadDelay)

import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Buttons
------------------------------------------------------------------------------}

main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
    addJavaScript w "https://cdn.datatables.net/1.11.3/js/jquery.dataTables.min.js"
    return w # set title "Buttons"
    UI.addStyleSheet w "buttons.css"
    addStyleSheetExt w "https://cdn.datatables.net/1.11.3/css/jquery.dataTables.min.css"

    buttons <- mkButtons
    let myTab = mkTable [string "H1", string "H2"]
                     [ [string "R1C1", string "R1C2"]
                     , [string "R2C1", string "R2C2"] ] # set UI.id_ "example" # set UI.class_ "display"  
                   
    dgScript <- mkElement  "Script" # set UI.text "$(document).ready(function() { $('#example').DataTable(); } );" 
    getBody w #+
        [UI.div #. "wrap" #+ (greet ++ [myTab, element dgScript] ++ map element buttons ++ [viewSource])]

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Hello, Haskell!"]
    , UI.div #+ [string "Try the buttons below, they hover and click."]
    ]


mkButton :: String -> UI (Element, Element)
mkButton title = do
    button <- UI.button #. "button" #+ [string title]
    view   <- UI.p #+ [element button]
    return (button, view)

mkButtons :: UI [Element]
mkButtons = do
    list    <- UI.ul #. "buttons-list"
    
    (button1, view1) <- mkButton button1Title
    
    on UI.hover button1 $ \_ -> do
        element button1 # set text (button1Title ++ " [hover]")
    on UI.leave button1 $ \_ -> do
        element button1 # set text button1Title
    on UI.click button1 $ \_ -> do
        element button1 # set text (button1Title ++ " [pressed]")
        liftIO $ threadDelay $ 1000 * 1000 * 1
        element list    #+ [UI.li # set html "<b>Delayed</b> result!"]
    
    (button2, view2) <- mkButton button2Title

    on UI.hover button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [hover]")
    on UI.leave button2 $ \_ -> do
        element button2 # set text button2Title
    on UI.click button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [pressed]")
        element list    #+ [UI.li # set html "Zap! Quick result!"]
    
    return [list, view1, view2]

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately"

viewSource :: UI Element
viewSource = UI.p #+
    [UI.anchor #. "view-source" # set UI.href url #+ [string "View source code"]]
    where
    url = samplesURL ++ "Buttons.hs"

addJavaScript :: UI.Window -> FilePath -> UI ()
addJavaScript w pathName = void $ do
  el <- UI.mkElement "script" # set UI.src pathName
  UI.getHead w #+ [UI.element el]

addStyleSheetExt :: Window -> FilePath -> UI ()
addStyleSheetExt w filename = void $ do
    el <- mkElement "link"
            # set (attr "rel" ) "stylesheet"
            # set (attr "type") "text/css"
            # set (attr "href") (filename)
    getHead w #+ [element el]  

mkTable    :: [UI Element]-> [[UI Element]] -> UI Element
mkTable mhead mrows = do
        rowHeader <- wrap "tr" =<< mapM (\entry -> wrap "th" [entry] ) =<< sequence mhead

        rows0 <- mapM sequence mrows

        rows  <- forM rows0 $ \row0 -> do
            wrap "tr" =<< forM row0 (\entry -> wrap "td" [entry])

        th <- wrap "thead" [rowHeader]    
        tb <- wrap "tbody" rows
 
        wrap "table" [th, tb]

    where
    wrap :: String -> [Element] -> UI Element    
    wrap c xs = mkElement c #+ map element xs  