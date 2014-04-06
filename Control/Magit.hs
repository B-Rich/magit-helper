{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Control.Magit where

import Conduit
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Function
import Data.Text as T
import Prelude hiding (log)
import Shelly
import System.IO

data MagitCommand
    = MagitHelp
    | MagitStatus Text
    deriving Show

data MagitResult
    = MagitError Text Text
    | MagitHelpText

class ToLisp a where
    toLisp :: a -> Text

instance ToLisp MagitResult where
    toLisp MagitHelpText = "\"Welcome to magit-helper!\""
    toLisp (MagitError err input) = T.concat
        [ "(error \"Parse failed: ", err
        , ", with input '", input, "'\")"
        ]

magitCommandLoop :: IO ()
magitCommandLoop = sourceHandle stdin
    $= linesUnboundedC
    $$ mapM_C $ \line -> do
        cmd <- case parseOnly parseCmd line of
            Left err -> return $ MagitError (pack err) line
            Right x  -> execute x
        yield cmd $$ lispConverter =$ sinkHandle stdout
  where
    parseCmd :: Parser MagitCommand
    parseCmd =
        MagitHelp <$ "help"

execute :: MagitCommand -> IO MagitResult
execute MagitHelp = return MagitHelpText

execute (MagitStatus dir) = do
    -- Produce the data which magit-status is going to ask for all at once,
    -- and return those results back to magit.
magit-cmd-insert-section: (diff-files)
magit-cmd-insert-section: (log --format=format:%h %s HEAD..refs/remotes/magit/master)
magit-cmd-insert-section: (log --format=format:%h %s refs/remotes/magit/master..HEAD)
magit-git-exit-code (diff --quiet --cached)
magit-git-exit-code (log -1 HEAD)
magit-git-exit-code (update-index --refresh)
magit-git-lines (config --get-all remote.magit.fetch)
magit-git-lines (stash list)
magit-git-lines (status --porcelain)
magit-git-string (config --bool branch.master.rebase)
magit-git-string (config branch.master.merge)
magit-git-string (config branch.master.remote)
magit-git-string (config core.abbrev)
magit-git-string (config remote.magit.url)
magit-git-string (describe --contains HEAD)
magit-git-string (describe --long --tags)
magit-git-string (describe --tags --dirty)
magit-git-string (log -1 --pretty=format:%h %s HEAD)
magit-git-string (rev-parse --git-dir)
magit-git-string (rev-parse --show-toplevel)
magit-git-string (rev-parse --verify HEAD)
magit-git-string (symbolic-ref -q HEAD)


git :: Text -> [Text] -> IO Text
git cmd args = shelly $ run "git" (cmd:args)

git_ :: Text -> [Text] -> IO ()
git_ cmd args = void $ git cmd args

-- | Convert incoming Haskell values to their Lisp representation, with all
--   the elements enclosed in outer-level parentheses.
lispConverter :: (ToLisp a, Monad m) => Conduit a m Text
lispConverter = do
    yield "(\n"
    awaitForever $ \v -> do
        yield $ toLisp v
        yield "\n"
    yield ")\n"
