{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import qualified Data.ByteString.Char8 as B
import System.Directory
import System.Console.CmdArgs
import System.Hardware.Serialport
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Text.Read (readMaybe)

data Options = Options
    { port :: FilePath
    , dir  :: FilePath
    } deriving (Show, Data, Typeable)

instance Default Options where def = Options "/dev/ttyACM0" "."

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

main :: IO ()
main = do
    Options{..} <- cmdArgs def
    withSerial port defaultSerialSettings { commSpeed = CS115200 } $ \port -> do
        liftIO $ setCurrentDirectory dir
        liftIO $ threadDelay 100000
        forever $ do
            getSerial port >>= mapM_ print . map readItem . lines . B.unpack

getSerial :: SerialPort -> IO B.ByteString
getSerial port = loop where
    loop = do
        x <- recv port 256
        if B.null x then return B.empty else B.append x `liftM` loop

