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
                             ) where

import Prelude
import System.FilePath ((<.>), (</>))
import Text.DocTemplates (Template, compileTemplate, renderTemplate)
import Text.Pandoc.Class (PandocMonad, readDataFile)
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Text (Text)

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

{-
-- | Create template Context from a 'Meta' and an association list
-- of variables, specified at the command line or in the writer.
-- Variables overwrite metadata fields with the same names.
-- If multiple variables are set with the same name, a list is
-- assigned.  Does nothing if 'writerTemplate' is Nothing.
metaToContext :: Monad m
              => WriterOptions
              -> ([Block] -> m a)
              -> ([Inline] -> m a)
              -> Meta
              -> m (Context a)
metaToContext opts blockWriter inlineWriter meta
  | isJust (writerTemplate opts) = addVariablesToContext opts <$>
      metaToContext' blockWriter inlineWriter meta
  | otherwise = return mempty

-- | Like 'metaToContext, but does not include variables and is
-- not sensitive to 'writerTemplate'.
metaToJSON' :: Monad m
           => ([Block] -> m a)
           -> ([Inline] -> m a)
           -> Meta
           -> m (Context a)
metaToContext' blockWriter inlineWriter (Meta metamap) = do
  renderedMap <- Traversable.mapM
                 (metaValueToVal blockWriter inlineWriter)
                 metamap
  return $ Context $ H.foldrWithKey defField mempty renderedMap

-- TODO: Previously addVariablesToJSON also added @meta-json@,
-- a field containing a string representation
-- of the original JSON object itself, prior to addition of variables.
-- We need to do that in some other way.  Perhaps we need to add
-- toText to our TemplateTarget class?

-- | Add variables to a template Context, replacing any existing values.
addVariablesToContext :: WriterOptions -> Context -> Context
addVariablesToContext opts context =
--  foldl (\acc (x,y) -> setField x y acc)
--       (defField "meta-json" (toStringLazy $ encode metadata) (Object mempty))
--       (writerVariables opts)
--    `combineMetadata` metadata
--  where combineMetadata (Object o1) (Object o2) = Object $ H.union o1 o2
--        combineMetadata x _                     = x

metaValueToVal :: Monad m
               => ([Block] -> m a)
               -> ([Inline] -> m a)
               -> MetaValue
               -> m (Val a)
-- metaValueToJSON blockWriter inlineWriter (MetaMap metamap) = toJSON <$>
--   Traversable.mapM (metaValueToJSON blockWriter inlineWriter) metamap
-- metaValueToJSON blockWriter inlineWriter (MetaList xs) = toJSON <$>
--   Traversable.mapM (metaValueToJSON blockWriter inlineWriter) xs
-- metaValueToJSON _ _ (MetaBool b) = return $ toJSON b
-- metaValueToJSON _ inlineWriter (MetaString s@('0':_:_)) =
--    -- don't treat string with leading 0 as string (#5479)
--    toJSON <$> inlineWriter (Builder.toList (Builder.text s))
-- metaValueToJSON _ inlineWriter (MetaString s) =
--   case safeRead s of
--      Just (n :: Scientific) -> return $ Aeson.Number n
--      Nothing -> toJSON <$> inlineWriter (Builder.toList (Builder.text s))
-- metaValueToJSON blockWriter _ (MetaBlocks bs) = toJSON <$> blockWriter bs
-- metaValueToJSON blockWriter inlineWriter (MetaInlines [Str s]) =
--   metaValueToJSON blockWriter inlineWriter (MetaString s)
-- metaValueToJSON _ inlineWriter (MetaInlines is) = toJSON <$> inlineWriter is

-- | Retrieve a field value from a Context.
getField :: String
         -> Context a
         -> Maybe a
getField field (Context hashmap) = do
--   result <- H.lookup (T.pack field) hashmap
--   case fromJSON result of
--        Success x -> return x
--        _         -> fail "Could not convert from JSON"

-- | Set a field of a Context.  If the field already has a value,
-- convert it into a list with the new value appended to the old value(s).
setField :: String
         -> a
         -> Context a
         -> Context a
setField field val (Context hashmap) =
--  Object $ H.insertWith combine (T.pack field) (toJSON val) hashmap
--  where combine newval oldval =
--          case fromJSON oldval of
--                Success xs -> toJSON $ xs ++ [newval]
                _          -> toJSON [oldval, newval]

-- | Reset a field of a Context.  If the field already has a value,
-- the new value replaces it.
resetField :: String
           -> a
           -> Context a
           -> Context a
resetField field val (Context hashmap) =
--  Object $ H.insert (T.pack field) (toJSON val) hashmap

-- | Set a field of a Context if it currently has no value.
-- If it has a value, do nothing.
defField :: String
         -> a
         -> Context a
         -> Context a
defField field val (Context hashmap) =
--  Object $ H.insertWith f (T.pack field) (toJSON val) hashmap
--    where f _newval oldval = oldval
-}

