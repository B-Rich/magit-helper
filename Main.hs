module Main where

import Control.Lens
import Control.Logging
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Thyme
import Options.Applicative
--import Options.Applicative.Types (ReadM(..))
import Prelude hiding (log)
import System.Environment

version :: String
version = "0.0.1"

copyright :: String
copyright = "2014"

summary :: String
summary = "magit-helper v" ++ version ++ ", (C) John Wiegley " ++ copyright

data Options = Options
    { verbose  :: Bool
    }

options :: Parser Options
options = Options
    <$> switch (long "verbose" <> help "Display statistics")
    -- <*> strOption (long "file" <> help "Active timelog file to use")
    -- <*> strOption (long "period" <> help "Period to report for" <> value "")

    -- <*> strOption (long "category"
    --                <> help "Account/category to query from timelog"
    --                <> value "")

    -- <*> strOption (long "archive" <> help "Archival timelog" <> value "")
    -- <*> option (long "gratis" <> help "Hours given free each month" <> value 0)

    -- <*> option (long "moment" <> help "Set notion of the current moment"
    --             <> value (unsafePerformIO $
    --                       (zonedTimeToLocalTime <$>) getZonedTime)
    --             <> reader (ReadM . Right . flip LocalTime midday . fromJust .
    --                        Atto.maybeResult .
    --                        Time.parseWithDefaultOptions Time.defaultDay .
    --                        B.pack))

type App = ReaderT Options IO

main :: IO ()
main = execParser opts >>= runReaderT main'
  where
    opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "Show hours worked so far"
                 <> header "hours - show hours worked so far")

main' :: App ()
main' = undefined
