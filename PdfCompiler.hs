module PdfCompiler (
  pdfCompile
) where

import Hakyll (Compiler, defaultHakyllWriterOptions, unsafeCompiler)

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Pandoc
import Text.Pandoc.PDF (makePDF)

-- Template variables (e.g. colorlinks, urlcolor) taken from post metadata
addVars :: [(String, String)] -> Pandoc -> Pandoc
addVars vars (Pandoc (Meta m) a) = Pandoc (Meta $ foldr ins m vars) a
  where ins (k, v) = M.insert (T.pack k) (MetaString (T.pack v))

pdfCompile :: String
           -> Maybe String
           -> [(String, String)]
           -> Pandoc
           -> Compiler ByteString
pdfCompile temp nsecs vars pan = unsafeCompiler $ runIOorExplode $ do
  template <- runWithDefaultPartials (compileTemplate "template.tex" (T.pack temp))
              >>= either (liftIO . throwIO . PandocTemplateError . T.pack) pure
  either id id <$> makePDF "pdflatex" [] writeLaTeX
      defaultHakyllWriterOptions {
          writerTemplate = Just template,
          writerNumberSections = isJust nsecs
      }
      (addVars vars pan)
