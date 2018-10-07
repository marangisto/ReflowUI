{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import Graphics.UI.Gtk
import Graphics.Rendering.Chart.Easy as C
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import qualified Data.ByteString.Char8 as B
import System.Directory
import System.Console.CmdArgs as CmdArgs
import System.Hardware.Serialport
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Text.Read (readMaybe)
import Data.Either
import Data.IORef

data Options = Options
    { port :: FilePath
    , dir  :: FilePath
    } deriving (Show, Data, Typeable)

instance CmdArgs.Default Options where def = Options "/dev/ttyACM0" "."

data State = Init | Ramp1 | Soak | Ramp2 | Reflow | Cool deriving (Show)

data Item = Item
    { state     :: State
    , time      :: Double
    , setPoint  :: Double
    , avgTemp   :: Double
    , tempA     :: Double
    , tempB     :: Double
    , power     :: Double
    } deriving (Show)

readItem :: String -> Either String Item
readItem s
    | (a:b:c:d:e:f:g:_) <- words s
    , Just state <- readState a
    , Just time <- readMaybe b
    , Just setPoint <- readMaybe c
    , Just avgTemp <- readMaybe d
    , Just tempA <- readMaybe e
    , Just tempB <- readMaybe f
    , Just power <- readMaybe g
    = Right $ Item{..}
    | otherwise = Left s
    where readState "init" = Just Init
          readState "ramp1" = Just Ramp1
          readState "soak" = Just Soak
          readState "ramp2" = Just Ramp2
          readState "reflow" = Just Reflow
          readState "cool" = Just Cool
          readState _ = Nothing

getSerial :: SerialPort -> IO B.ByteString
getSerial port = loop where
    loop = do
        x <- recv port 256
        if B.null x then return B.empty else B.append x `liftM` loop

signal :: [Double] -> [(Double, Double)]
signal _ = [ (x, sin x) | x <- [0.0,0.05..2*pi] ]

plotItems :: [Item] -> Renderable ()
plotItems xs = toRenderable $ do
    layoutlr_title .= "Reflow Profile"
    setColors
        [ opaque black
        , opaque red
        , opaque green
        , opaque blue
        , withOpacity gray 0.5
        ]
    plotLeft (line "set-point" [ [ (time, setPoint) | Item{..} <- xs ] ])
    plotLeft (line "avg-temp" [ [ (time, avgTemp) | Item{..} <- xs ] ])
    plotLeft (line "temp-A" [ [ (time, tempA) | Item{..} <- xs ] ])
    plotLeft (line "temp-B" [ [ (time, tempB) | Item{..} <- xs ] ])
    plotRight (line "power" [ [ (time, power) | Item{..} <- xs ] ])

renderScene :: DrawingArea -> IO Bool
renderScene da = do
    updateCanvas (plotItems []) da

readController :: FilePath -> DrawingArea -> IO ()
readController port drawingArea = do
    items <- newIORef []
    withSerial port defaultSerialSettings { commSpeed = CS115200 } $ \port -> do
        liftIO $ threadDelay 100000
        forever $ do
            (xs, ys) <- (partitionEithers . map readItem .  lines . B.unpack) <$> getSerial port
            unless (null xs) $ mapM_ putStrLn xs
            unless (null ys) $ do
                modifyIORef' items (++ys)
                zs <- readIORef items
                postGUIAsync $ void $ updateCanvas (plotItems zs) drawingArea
                print =<< (length <$> readIORef items)

main :: IO ()
main = do
    Options{..} <- cmdArgs CmdArgs.def
    setCurrentDirectory dir
    initGUI
    window <- windowNew
    drawingArea <- drawingAreaNew
    containerAdd window drawingArea
    window `onDestroy` mainQuit
    windowSetDefaultSize window 640 480
    widgetShowAll window
    forkIO $ readController port drawingArea
    mainGUI

