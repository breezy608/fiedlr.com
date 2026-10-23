module PdfCompiler (
  pdfCompile
) where

import Hakyll (Compiler, defaultHakyllWriterOptions, unsafeCompiler)

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T

import Text.Pandoc
import Text.Pandoc.PDF (makePDF)

pdfCompile :: String
           -> Maybe String
           -> Pandoc
           -> Compiler ByteString
pdfCompile temp nsecs pan = unsafeCompiler $ runIOorExplode $ do
  template <- runWithDefaultPartials (compileTemplate "template.tex" (T.pack temp))
              >>= either (liftIO . throwIO . PandocTemplateError . T.pack) pure
  either id id <$> makePDF "pdflatex" [] writeLaTeX
      defaultHakyllWriterOptions {
          writerTemplate = Just template,
          writerNumberSections = isJust nsecs
      }
      pan
