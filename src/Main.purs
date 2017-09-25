module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Aff.Console (error, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (runExcept)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, keys, singleton)
import Data.String (drop)
import Node.Process (argv)
import Simple.JSON (read, writeJSON)
import Text.Prettier (Parser(..), defaultOptions, format)

type Result =
  { name :: String
  , latest ::
      { dependencies :: StrMap String
      , version :: String -- probably needs v prefix applied after
      , repository ::
          { url :: String
          }
      }
  }

data Output = Output String
  { dependencies :: Array String
  , repo :: String
  , version :: String
  }

resultToOutput :: Result -> Output
resultToOutput result = Output
  result.name
  { repo: result.latest.repository.url
  , version: "v" <> result.latest.version
  , dependencies
  }
  where
    dependencies =
      drop 11 <$> keys result.latest.dependencies

foreign import _getLatest :: forall e
   . String
  -> (Error -> Eff e Unit)
  -> (Foreign -> Eff e Unit)
  -> Eff e Unit

getLatest :: forall e. String -> Aff e Foreign
getLatest name = makeAff $ _getLatest name

main :: _
main = void $ launchAff do
  args <- liftEff $ argv
  case args !! 2 of
    Just name -> do
      o <-  map resultToOutput <<< read <$> getLatest name
      case runExcept o of
        Right (Output name spec) -> do
          let
            output = singleton (drop 11 name) spec
          log $ format (defaultOptions {parser = JSON}) $ writeJSON output
        Left e -> do
          error $ "error occured: " <> show e
    Nothing -> do
      error "No package name provided. Please give us one!"
