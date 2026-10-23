{-# LANGUAGE OverloadedStrings          #-}
module BiblioCompiler (
  pandocRead,
  pandocCompile,
  cslTemplate
) where

import Hakyll
import System.FilePath
import Data.Maybe (isJust)
import qualified Data.Map               as M
import qualified Data.Text              as T
import           Text.Pandoc

cslTemplate :: String
cslTemplate = "default"

cslFilePath :: FilePath
cslFilePath = "csl" </> cslTemplate <.> "csl"

addMeta :: T.Text -> MetaValue -> Pandoc -> Pandoc
addMeta k v (Pandoc (Meta m) a) = Pandoc (Meta $ M.insert k v m) a

addLinkCitations :: Pandoc -> Pandoc
addLinkCitations = addMeta "link-citations"          (MetaBool True)
                 . addMeta "reference-section-title" (MetaString "References")

-- Join soft line breaks into spaces like pandoc < 2.17 did
baseWriterOptions :: WriterOptions
baseWriterOptions = defaultHakyllWriterOptions { writerWrapText = WrapNone }

writerOptions :: Maybe String -> WriterOptions
writerOptions Nothing = baseWriterOptions
writerOptions _  = baseWriterOptions {
    writerExtensions = writerExtensions baseWriterOptions <> extensionsFromList [
        Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros, -- math
        Ext_backtick_code_blocks, Ext_fenced_code_attributes, -- code
        Ext_auto_identifiers, -- references
        Ext_yaml_metadata_block -- additional support for metadata
    ],
    -- Doublecheck this change, because in Hakyll documentation there is still writerHTMLMathMethod
    writerMathMethod = MathJax ""
}

-- Although I've edited the code quite a bit,
-- all thanks go to vjeranc (https://github.com/jaspervdj/hakyll/issues/471)
readPandocBiblioWithTransform :: ReaderOptions
                              -> Item CSL
                              -> Item Biblio
                              -> (Pandoc -> Pandoc)
                              -> Item String
                              -> Compiler (Item Pandoc)
readPandocBiblioWithTransform ropt csl biblio f item =
    readPandocWith ropt item >>= processPandocBiblio csl biblio . fmap f

pandocBiblioCompilerWithTransform :: ReaderOptions -> WriterOptions
                                  -> String -- BIB FilePath
                                  -> (Pandoc -> Pandoc)
                                  -> Compiler (Item String)
pandocBiblioCompilerWithTransform ropt wopt bibFileName f = do
    csl <- load $ fromFilePath cslFilePath
    bib <- load $ fromFilePath bibFileName
    writePandocWith wopt <$>
        (getResourceBody >>= readPandocBiblioWithTransform ropt csl bib f)

pandocRead :: Maybe (Item Biblio)  -- use Citations ?
           -> (Item String -> Compiler (Item Pandoc))
pandocRead Nothing = readPandoc
pandocRead (Just bib) = \item -> do
    csl <- load $ fromFilePath cslFilePath
    readPandocBiblioWithTransform
        defaultHakyllReaderOptions
        csl
        bib
        addLinkCitations
        item

pandocCompile :: Maybe String -- use MathJax?
              -> Maybe String -- use Citations?
              -> Maybe String -- number sections?
              -> Compiler (Item String)
pandocCompile mathJax biblioFile nsecs =
    maybe (
        pandocCompilerWith defaultHakyllReaderOptions
        ((writerOptions mathJax) { writerNumberSections = isJust nsecs })
    )
    (\biblioFileName ->
        pandocBiblioCompilerWithTransform defaultHakyllReaderOptions
            ((writerOptions mathJax) { writerNumberSections = isJust nsecs })
            ("bib" </> biblioFileName <.> "bib")
            addLinkCitations
    ) biblioFile
