module Myocardio.Options(Command(..), parser, parseOpts) where

import Data.Monoid
  ( mempty,
    (<>),
  )
import Data.Text (Text)
import Options.Applicative
  ( Parser,
    command,
    execParser,
    fullDesc,
    helper,
    info,
    many,
    progDesc,
    strArgument,
    subparser,
    (<**>),
  )
import Text.Show (Show)
import Prelude()
import System.IO(IO)
import Control.Applicative(pure)
import Data.Functor((<$>))

data Command = CommandList | CommandTag [Text] | CommandCommit deriving (Show)

parser :: Parser Command
parser =
  subparser
    ( command "list" (info (pure CommandList) (progDesc "List exercises"))
        <> command
          "commit"
          (info (pure CommandCommit) (progDesc "Commit exercises"))
        <> command
          "tag"
          ( info
              (CommandTag <$> many (strArgument mempty))
              (progDesc "Commit exercises")
          )
    )

parseOpts :: IO Command
parseOpts = execParser (info (parser <**> helper) fullDesc)
