{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Templates
   Copyright   : Copyright (C) 2009-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Utility functions for working with pandoc templates.
-}

module Text.Pandoc.Templates ( Template
                             , compileTemplate
                             , renderTemplate
                             , getDefaultTemplate
                             , metaToContext
                             , metaToContext'
                             , defField
                             , setField
                             , getField
                             , resetField
                             ) where

import Prelude
import System.FilePath ((<.>), (</>))
import Text.DocTemplates (Template, compileTemplate, renderTemplate, Context(..),
                         FromContext(..), Val(..), TemplateTarget(..))
import Text.Pandoc.Class (PandocMonad, readDataFile)
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as Builder
import Text.Pandoc.Options (WriterOptions(..))

-- | Get default template for the specified writer.
getDefaultTemplate :: PandocMonad m
                   => String           -- ^ Name of writer
                   -> m Text
getDefaultTemplate writer = do
  let format = takeWhile (`notElem` ("+-" :: String)) writer  -- strip off extensions
  case format of
       "native"  -> return ""
       "json"    -> return ""
       "docx"    -> return ""
       "fb2"     -> return ""
       "pptx"    -> return ""
       "ipynb"   -> return ""
       "odt"     -> getDefaultTemplate "opendocument"
       "html"    -> getDefaultTemplate "html5"
       "docbook" -> getDefaultTemplate "docbook5"
       "epub"    -> getDefaultTemplate "epub3"
       "beamer"  -> getDefaultTemplate "latex"
       "markdown_strict"   -> getDefaultTemplate "markdown"
       "multimarkdown"     -> getDefaultTemplate "markdown"
       "markdown_github"   -> getDefaultTemplate "markdown"
       "markdown_mmd"      -> getDefaultTemplate "markdown"
       "markdown_phpextra" -> getDefaultTemplate "markdown"
       "gfm"               -> getDefaultTemplate "commonmark"
       _        -> do
         let fname = "templates" </> "default" <.> format
         UTF8.toText <$> readDataFile fname

-- | Create template Context from a 'Meta' and an association list
-- of variables, specified at the command line or in the writer.
-- Variables overwrite metadata fields with the same names.
-- If multiple variables are set with the same name, a list is
-- assigned.  Does nothing if 'writerTemplate' is Nothing.
metaToContext :: (Monad m, TemplateTarget a)
              => WriterOptions
              -> ([Block] -> m a)
              -> ([Inline] -> m a)
              -> Meta
              -> m (Context a)
metaToContext opts blockWriter inlineWriter meta =
  case writerTemplate opts of
    Nothing -> return mempty
    Just _  -> addVariablesToContext opts <$>
                metaToContext' blockWriter inlineWriter meta

-- | Like 'metaToContext, but does not include variables and is
-- not sensitive to 'writerTemplate'.
metaToContext' :: (Monad m, TemplateTarget a)
           => ([Block] -> m a)
           -> ([Inline] -> m a)
           -> Meta
           -> m (Context a)
metaToContext' blockWriter inlineWriter (Meta metamap) = do
  renderedMap <- mapM (metaValueToVal blockWriter inlineWriter) metamap
  return $ Context
         $ M.foldrWithKey (\k v x -> M.insert (T.pack k) v x) mempty $ renderedMap

-- TODO: Previously addVariablesToJSON also added @meta-json@,
-- a field containing a string representation
-- of the original JSON object itself, prior to addition of variables.
-- We need to do that in some other way.  Perhaps we need to add
-- toText to our TemplateTarget class?

-- | Add variables to a template Context, replacing any existing values.
addVariablesToContext :: TemplateTarget a
                      => WriterOptions -> Context a -> Context a
addVariablesToContext opts = combineMetadata
  (foldl (\acc (x,y) ->
      setField x (fromText $ T.pack y) acc) mempty (writerVariables opts))
 where
  combineMetadata (Context o1) (Context o2) = Context $ M.union o1 o2

metaValueToVal :: (Monad m, TemplateTarget a)
               => ([Block] -> m a)
               -> ([Inline] -> m a)
               -> MetaValue
               -> m (Val a)
metaValueToVal blockWriter inlineWriter (MetaMap metamap) =
  MapVal . Context . M.mapKeys T.pack  <$>
    mapM (metaValueToVal blockWriter inlineWriter) metamap
metaValueToVal blockWriter inlineWriter (MetaList xs) = ListVal <$>
  mapM (metaValueToVal blockWriter inlineWriter) xs
metaValueToVal _ _ (MetaBool True) = return $ SimpleVal $ fromText "true"
metaValueToVal _ _ (MetaBool False) = return NullVal
metaValueToVal _ inlineWriter (MetaString s) =
   SimpleVal <$> inlineWriter (Builder.toList (Builder.text s))
metaValueToVal blockWriter _ (MetaBlocks bs) = SimpleVal <$> blockWriter bs
metaValueToVal _ inlineWriter (MetaInlines is) = SimpleVal <$> inlineWriter is

-- | Retrieve a field value from a Context.
getField :: FromContext a b
         => String
         -> Context a
         -> Maybe b
getField field (Context hashmap) =
  M.lookup (T.pack field) hashmap >>= fromVal

-- | Set a field of a Context.  If the field already has a value,
-- convert it into a list with the new value appended to the old value(s).
setField :: String
         -> a
         -> Context a
         -> Context a
setField field val (Context m) =
  Context $ M.insertWith combine (T.pack field) (SimpleVal val) m
 where
  combine newval (ListVal xs)   = ListVal (xs ++ [newval])
  combine newval x              = ListVal [x, newval]

-- | Reset a field of a Context.  If the field already has a value,
-- the new value replaces it.
resetField :: String
           -> a
           -> Context a
           -> Context a
resetField field val (Context m) =
  Context (M.insert (T.pack field) (SimpleVal val) m)

-- | Set a field of a Context if it currently has no value.
-- If it has a value, do nothing.
defField :: String
         -> a
         -> Context a
         -> Context a
defField field val (Context m) =
  Context (M.insertWith f (T.pack field) (SimpleVal val) m)
  where
    f _newval oldval = oldval
