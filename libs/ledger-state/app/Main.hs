{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State.Query
import Cardano.Ledger.State.UTxO
import Control.Monad
import Data.Text as T (pack)
import Options.Applicative
import System.IO

data Opts = Opts
  { -- | Path to the CBOR encoded NewEpochState data type, which will be used to
    -- load into sqlite database
    optsNewEpochStateBinaryFile :: Maybe FilePath,
    -- | Path to the CBOR encoded NewEpochState data type, which will be used to
    -- load into sqlite database
    optsEpochStateBinaryFile :: Maybe FilePath,
    -- | Path to Sqlite database file.
    optsSqliteDbFile :: Maybe FilePath
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> option
      (Just <$> str)
      ( long "new-epoch-state-cbor"
          <> value Nothing
          <> help
            ( "Path to the CBOR encoded NewEpochState data type. "
                <> "Can be produced by `cardano-cli query ledger-state` command. "
                <> "When supplied stats about the state will be printed to stdout"
            )
      )
    <*> option
      (Just <$> str)
      ( long "epoch-state-cbor"
          <> value Nothing
          <> help
            ( "Path to the CBOR encoded NewEpochState data type. "
                <> "Can be produced by `cardano-cli query ledger-state` command. "
                <> "When supplied stats about the state will be printed to stdout"
            )
      )
    <*> option
      (Just <$> str)
      ( long "sqlite-db"
          <> value Nothing
          <> help
            ( "Path to Sqlite database file. When supplied then new-epoch-state "
                <> "will be loaded into the databse. Requires --new-epoch-state-cbor"
            )
      )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <-
    execParser $
      info
        ( optsParser
            <* abortOption
              (ShowHelpText Nothing)
              (long "help" <> short 'h' <> help "Display this message.")
        )
        (header "ledger-state - Tool for analyzing ledger state")
  forM_ (optsNewEpochStateBinaryFile opts) $ \binFp -> do
    nes <- readNewEpochState binFp
    forM_ (optsSqliteDbFile opts) $ \dbFpStr -> do
      let dbFp = T.pack dbFpStr
      storeEpochState dbFp $ nesEs nes
      putStrLn "Loaded NewEpochState into the database"
    printNewEpochStateStats $ countNewEpochStateStats nes
  forM_ (optsEpochStateBinaryFile opts) $ \binFp -> do
    forM_ (optsSqliteDbFile opts) $ \dbFpStr -> do
      let dbFp = T.pack dbFpStr
      epochState <- loadEpochState dbFp
      putStrLn "Written EpochState into the database"
      writeEpochState binFp epochState
  -- forM_ (optsSqliteDbFile opts) $ \dbFpStr -> do
  --   let dbFp = T.pack dbFpStr
  --   km <- loadDbUTxO txIdSharingKeyMap dbFp
  --   m <- loadDbUTxO noSharing dbFp
  --   testKeyMap km m
