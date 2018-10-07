{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import qualified Data.ByteString.Char8 as B
import System.Directory
import System.Console.CmdArgs
import System.Hardware.Serialport
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad

data Options = Options
    { port :: FilePath
    , dir  :: FilePath
    } deriving (Show, Data, Typeable)

instance Default Options where def = Options "/dev/ttyACM0" "."

main :: IO ()
main = do
    Options{..} <- cmdArgs def
    withSerial port defaultSerialSettings { commSpeed = CS115200 } $ \port -> do
        liftIO $ setCurrentDirectory dir
        liftIO $ threadDelay 100000
        forever $ do
            getSerial port >>= putStr . B.unpack

getSerial :: SerialPort -> IO B.ByteString
getSerial port = loop where
    loop = do
        x <- recv port 256
        if B.null x then return B.empty else B.append x `liftM` loop

