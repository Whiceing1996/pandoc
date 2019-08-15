{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{- |
   Module      : Text.Pandoc.Pretty
   Copyright   : Copyright (C) 2010-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A prettyprinting library for the production of text documents,
including wrapped text, indentated blocks, and tables.
-}

module Text.Pandoc.Pretty (
       Doc
     , render
     , cr
     , blankline
     , blanklines
     , space
     , text
     , char
     , prefixed
     , flush
     , nest
     , hang
     , beforeNonBlank
     , nowrap
     , afterBreak
     , offset
     , minOffset
     , height
     , lblock
     , cblock
     , rblock
     , (<>)
     , (<+>)
     , ($$)
     , ($+$)
     , isEmpty
     , empty
     , cat
     , hcat
     , hsep
     , vcat
     , vsep
     , nestle
     , chomp
     , inside
     , braces
     , brackets
     , parens
     , quotes
     , doubleQuotes
     , charWidth
     , realLength
     )

where
import Prelude
import Control.Monad
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.List (intersperse, foldl')
import Data.Sequence (Seq, ViewL (..), fromList, singleton, viewl, (<|))
import qualified Data.Sequence as Seq
import Data.String
import qualified Text.DocTemplates as DT
import qualified Data.Text as T

data RenderState a = RenderState{
         output     :: [a]        -- ^ In reverse order
       , prefix     :: String
       , usePrefix  :: Bool
       , lineLength :: Maybe Int  -- ^ 'Nothing' means no wrapping
       , column     :: Int
       , newlines   :: Int        -- ^ Number of preceding newlines
       }

type DocState a = State (RenderState a) ()

data D a = Text Int a
         | Block Int [a]
         | Prefixed String (D a)
         | BeforeNonBlank (D a)
         | Flush (D a)
         | BreakingSpace
         | AfterBreak a
         | CarriageReturn
         | NewLine
         | BlankLines Int  -- number of blank lines
         | Concat (D a) (D a)
         | Empty
         deriving (Show, Eq, Functor, Foldable, Traversable)

instance Semigroup (D a) where
  x <> Empty = x
  Empty <> x = x
  x <> y     = Concat x y

instance Monoid (D a) where
  mappend = (<>)
  mempty = Empty

type Doc = D String
-- newtype Doc = Doc { unDoc :: Seq (D String) }
--              deriving (Semigroup, Monoid, Show, Eq)

instance IsString a => IsString (D a) where
  fromString = text

instance DT.TemplateTarget (D String) where
  fromText = text . T.unpack
  removeFinalNewline = id
  nested = nest
  isEmpty = isEmpty

unfoldD :: D a -> [D a]
unfoldD Empty = []
unfoldD (Concat x@(Concat{}) y) = unfoldD x <> unfoldD y
unfoldD (Concat x y)            = x : unfoldD y
unfoldD x                       = [x]

isBlank :: D a -> Bool
isBlank BreakingSpace  = True
isBlank CarriageReturn = True
isBlank NewLine        = True
isBlank (BlankLines _) = True
-- isBlank (Text _ (c:_)) = isSpace c
isBlank _              = False

-- | True if the document is empty.
isEmpty :: Doc -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | The empty document.
empty :: Doc
empty = mempty

-- | Concatenate a list of 'Doc's.
cat :: [Doc] -> Doc
cat = mconcat

-- | Same as 'cat'.
hcat :: [Doc] -> Doc
hcat = mconcat

-- | Concatenate a list of 'Doc's, putting breakable spaces
-- between them.
infixr 6 <+>
(<+>) :: Doc -> Doc -> Doc
(<+>) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> space <> y

-- | Same as 'cat', but putting breakable spaces between the
-- 'Doc's.
hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty

infixr 5 $$
-- | @a $$ b@ puts @a@ above @b@.
($$) :: Doc -> Doc -> Doc
($$) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> cr <> y

infixr 5 $+$
-- | @a $+$ b@ puts @a@ above @b@, with a blank line between.
($+$) :: Doc -> Doc -> Doc
($+$) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> blankline <> y

-- | List version of '$$'.
vcat :: [Doc] -> Doc
vcat = foldr ($$) empty

-- | List version of '$+$'.
vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

-- | Removes leading blank lines from a 'Doc'.
nestle :: D a -> D a
nestle d =
  case d of
    BlankLines _              -> Empty
    NewLine                   -> Empty
    Concat (Concat x y) z     -> nestle (Concat x (Concat y z))
    Concat BlankLines{} x     -> nestle x
    Concat NewLine x          -> nestle x
    _                         -> d

-- | Chomps trailing blank space off of a 'Doc'.
chomp :: D a -> D a
chomp d =
    case d of
    BlankLines _              -> Empty
    NewLine                   -> Empty
    CarriageReturn            -> Empty
    BreakingSpace             -> Empty
    Prefixed s d'             -> Prefixed s (chomp d')
    Concat (Concat x y) z     -> chomp (Concat x (Concat y z))
    Concat x y                ->
        case chomp y of
          Empty -> chomp x
          z     -> x <> z
    _                         -> d

outp :: (IsString a) => Int -> String -> DocState a
outp off s | off < 0 = do  -- offset < 0 means newline characters
  st' <- get
  let rawpref = prefix st'
  when (column st' == 0 && usePrefix st' && not (null rawpref)) $ do
    let pref = reverse $ dropWhile isSpace $ reverse rawpref
    modify $ \st -> st{ output = fromString pref : output st
                      , column = column st + realLength pref }
  let numnewlines = length $ takeWhile (=='\n') $ reverse s
  modify $ \st -> st { output = fromString s : output st
                     , column = 0
                     , newlines = newlines st + numnewlines }
outp off s = do           -- offset >= 0 (0 might be combining char)
  st' <- get
  let pref = prefix st'
  when (column st' == 0 && usePrefix st' && not (null pref)) $
    modify $ \st -> st{ output = fromString pref : output st
                    , column = column st + realLength pref }
  modify $ \st -> st{ output = fromString s : output st
                    , column = column st + off
                    , newlines = 0 }

-- | Renders a 'Doc'.  @render (Just n)@ will use
-- a line length of @n@ to reflow text on breakable spaces.
-- @render Nothing@ will not reflow text.
render :: (IsString a) => Maybe Int -> Doc -> a
render linelen doc = fromString . mconcat . reverse . output $
  execState (renderDoc doc) startingState
   where startingState = RenderState{
                            output = mempty
                          , prefix = ""
                          , usePrefix = True
                          , lineLength = linelen
                          , column = 0
                          , newlines = 2 }

renderDoc :: (IsString a, Monoid a)
          => Doc -> DocState a
renderDoc = renderList . dropWhile (== BreakingSpace) . unfoldD

data IsBlock = IsBlock Int [String]

-- This would be nicer with a pattern synonym
-- pattern VBlock i s <- mkIsBlock -> Just (IsBlock ..)

renderList :: (IsString a, Monoid a)
           => [D String] -> DocState a
renderList [] = return ()
renderList (Text off s : xs) = do
  outp off s
  renderList xs

renderList (Prefixed pref d : xs) = do
  st <- get
  let oldPref = prefix st
  put st{ prefix = prefix st ++ pref }
  renderDoc d
  modify $ \s -> s{ prefix = oldPref }
  renderList xs

renderList (Flush d : xs) = do
  st <- get
  let oldUsePrefix = usePrefix st
  put st{ usePrefix = False }
  renderDoc d
  modify $ \s -> s{ usePrefix = oldUsePrefix }
  renderList xs

renderList (BeforeNonBlank d : xs) =
  case xs of
    (x:_) | isBlank x -> renderList xs
          | otherwise -> renderDoc d >> renderList xs
    []                -> renderList xs

renderList [BlankLines _] = return ()

renderList (BlankLines m : BlankLines n : xs) =
  renderList (BlankLines (max m n) : xs)

renderList (BlankLines num : BreakingSpace : xs) =
  renderList (BlankLines num : xs)

renderList (BlankLines num : xs) = do
  st <- get
  case output st of
     _ | newlines st > num -> return ()
       | otherwise -> replicateM_ (1 + num - newlines st) (outp (-1) "\n")
  renderList xs

renderList (CarriageReturn : BlankLines m : xs) =
  renderList (BlankLines m : xs)

renderList (CarriageReturn : BreakingSpace : xs) =
  renderList (CarriageReturn : xs)

renderList (CarriageReturn : xs) = do
  st <- get
  if newlines st > 0 || null xs
     then renderList xs
     else do
       outp (-1) "\n"
       renderList xs

renderList (NewLine : xs) = do
  outp (-1) "\n"
  renderList xs

renderList (BreakingSpace : CarriageReturn : xs) =
  renderList (CarriageReturn:xs)
renderList (BreakingSpace : NewLine : xs) = renderList (NewLine:xs)
renderList (BreakingSpace : BlankLines n : xs) = renderList (BlankLines n:xs)
renderList (BreakingSpace : BreakingSpace : xs) = renderList (BreakingSpace:xs)
renderList (BreakingSpace : xs) = do
  let isText (Text _ _)     = True
      isText (Block _ _)    = True
      isText (AfterBreak _) = True
      isText _              = False
  let isBreakingSpace BreakingSpace = True
      isBreakingSpace _             = False
  let xs' = dropWhile isBreakingSpace xs
  let next = takeWhile isText xs'
  st <- get
  let off = foldl' (+) 0 $ map offsetOf next
  case lineLength st of
        Just l | column st + 1 + off > l -> do
          outp (-1) "\n"
          renderList xs'
        _  -> do
          outp 1 " "
          renderList xs'

renderList (AfterBreak s : xs) = do
  st <- get
  when (newlines st > 0) $ outp (realLength s) s
  renderList xs

renderList (Block i1 s1 : Block i2 s2  : xs) =
  renderList (mergeBlocks False (IsBlock i1 s1) (IsBlock i2 s2) : xs)

renderList (Block i1 s1 : BreakingSpace : Block i2 s2 : xs) =
  renderList (mergeBlocks True (IsBlock i1 s1) (IsBlock i2 s2) : xs)

renderList (Block _width lns : xs) = do
  st <- get
  let oldPref = prefix st
  case column st - realLength oldPref of
        n | n > 0 -> modify $ \s -> s{ prefix = oldPref ++ replicate n ' ' }
        _ -> return ()
  renderList $ intersperse CarriageReturn (map (Text 0) lns)
  modify $ \s -> s{ prefix = oldPref }
  renderList xs

mergeBlocks :: Bool -> IsBlock -> IsBlock -> D String
mergeBlocks addSpace (IsBlock w1 lns1) (IsBlock w2 lns2) =
  Block (w1 + w2 + if addSpace then 1 else 0) $
     zipWith (\l1 l2 -> pad w1 l1 ++ l2) lns1' (map sp lns2')
    where (lns1', lns2') = case (length lns1, length lns2) of
                                (x, y) | x > y -> (lns1,
                                                   lns2 ++ replicate (x - y) "")
                                       | x < y -> (lns1 ++ replicate (y - x) "",
                                                   lns2)
                                       | otherwise -> (lns1, lns2)
          pad n s = s ++ replicate (n - realLength s) ' '
          sp "" = ""
          sp xs = if addSpace then ' ' : xs else xs

offsetOf :: D String -> Int
offsetOf (Text o _)    = o
offsetOf (Block w _)   = w
offsetOf BreakingSpace = 1
offsetOf _             = 0

-- | A literal string.
text :: IsString a => String -> D a
text [] = mempty
text s = case break (=='\n') s of
           ([], _:ys) -> NewLine <> text ys
           (xs, _:ys) -> Text (realLength xs) (fromString xs) <>
                             (NewLine <> text ys)
           (xs, [])   -> Text (realLength xs) (fromString xs)

-- | A character.
char :: IsString a => Char -> D a
char c = text [c]

-- | A breaking (reflowable) space.
space :: D a
space = BreakingSpace

-- | A carriage return.  Does nothing if we're at the beginning of
-- a line; otherwise inserts a newline.
cr :: D a
cr = CarriageReturn

-- | Inserts a blank line unless one exists already.
-- (@blankline <> blankline@ has the same effect as @blankline@.
blankline :: D a
blankline = BlankLines 1

-- | Inserts blank lines unless they exist already.
-- (@blanklines m <> blanklines n@ has the same effect as @blanklines (max m n)@.
blanklines :: Int -> D a
blanklines n = BlankLines n

-- | Uses the specified string as a prefix for every line of
-- the inside document (except the first, if not at the beginning
-- of the line).
prefixed :: String -> D a -> D a
prefixed pref doc = Prefixed pref doc

-- | Makes a 'Doc' flush against the left margin.
flush :: D a -> D a
flush doc = Flush doc

-- | Indents a 'Doc' by the specified number of spaces.
nest :: Int -> D a -> D a
nest ind = prefixed (replicate ind ' ')

-- | A hanging indent. @hang ind start doc@ prints @start@,
-- then @doc@, leaving an indent of @ind@ spaces on every
-- line but the first.
hang :: Int -> D a -> D a -> D a
hang ind start doc = start <> nest ind doc

-- | @beforeNonBlank d@ conditionally includes @d@ unless it is
-- followed by blank space.
beforeNonBlank :: D a -> D a
beforeNonBlank d = BeforeNonBlank d

-- | Makes a 'Doc' non-reflowable.
nowrap :: IsString a => D a -> D a
nowrap doc = mconcat . map replaceSpace . unfoldD $ doc
  where replaceSpace BreakingSpace = Text 1 $ fromString " "
        replaceSpace x             = x

-- | Content to print only if it comes at the beginning of a line,
-- to be used e.g. for escaping line-initial `.` in roff man.
afterBreak :: a -> D a
afterBreak d = AfterBreak d

-- | Returns the width of a 'Doc'.
offset :: D String -> Int
offset d = maximum (0: map realLength (lines $ render Nothing d))

-- | Returns the minimal width of a 'Doc' when reflowed at breakable spaces.
minOffset :: D String -> Int
minOffset d = maximum (0: map realLength (lines $ render (Just 0) d))

-- | @lblock n d@ is a block of width @n@ characters, with
-- text derived from @d@ and aligned to the left.
lblock :: Int -> D String -> D String
lblock = block id

-- | Like 'lblock' but aligned to the right.
rblock :: Int -> D String -> D String
rblock w = block (\s -> replicate (w - realLength s) ' ' <> s) w

-- | Like 'lblock' but aligned centered.
cblock :: Int -> D String -> D String
cblock w = block (\s -> replicate ((w - realLength s) `div` 2) ' ' <> s) w

-- | Returns the height of a block or other 'Doc'.
height :: D String -> Int
height = length . lines . render Nothing

block :: (String -> String) -> Int -> D String -> D String
block filler width d
  | width < 1 && not (isEmpty d) = block filler 1 d
  | otherwise                    = Block width $ map filler
                                 $ chop width $ render (Just width) d

chop :: Int -> String -> [String]
chop _ [] = []
chop n cs = case break (=='\n') cs of
                  (xs, ys)     -> if len <= n
                                     then case ys of
                                             []     -> [xs]
                                             ['\n'] -> [xs]
                                             (_:zs) -> xs : chop n zs
                                     else take n xs : chop n (drop n xs ++ ys)
                                   where len = realLength xs

-- | Encloses a 'Doc' inside a start and end 'Doc'.
inside :: D a -> D a -> D a -> D a
inside start end contents =
  start <> contents <> end

-- | Puts a 'Doc' in curly braces.
braces :: IsString a => D a -> D a
braces = inside (char '{') (char '}')

-- | Puts a 'Doc' in square brackets.
brackets :: IsString a => D a -> D a
brackets = inside (char '[') (char ']')

-- | Puts a 'Doc' in parentheses.
parens :: IsString a => D a -> D a
parens = inside (char '(') (char ')')

-- | Wraps a 'Doc' in single quotes.
quotes :: IsString a => D a -> D a
quotes = inside (char '\'') (char '\'')

-- | Wraps a 'Doc' in double quotes.
doubleQuotes :: IsString a => D a -> D a
doubleQuotes = inside (char '"') (char '"')

-- | Returns width of a character in a monospace font:  0 for a combining
-- character, 1 for a regular character, 2 for an East Asian wide character.
charWidth :: Char -> Int
charWidth c =
  case c of
      _ | c <  '\x0300'                    -> 1
        | c >= '\x0300' && c <= '\x036F'   -> 0  -- combining
        | c >= '\x0370' && c <= '\x10FC'   -> 1
        | c >= '\x1100' && c <= '\x115F'   -> 2
        | c >= '\x1160' && c <= '\x11A2'   -> 1
        | c >= '\x11A3' && c <= '\x11A7'   -> 2
        | c >= '\x11A8' && c <= '\x11F9'   -> 1
        | c >= '\x11FA' && c <= '\x11FF'   -> 2
        | c >= '\x1200' && c <= '\x2328'   -> 1
        | c >= '\x2329' && c <= '\x232A'   -> 2
        | c >= '\x232B' && c <= '\x2E31'   -> 1
        | c >= '\x2E80' && c <= '\x303E'   -> 2
        | c == '\x303F'                    -> 1
        | c >= '\x3041' && c <= '\x3247'   -> 2
        | c >= '\x3248' && c <= '\x324F'   -> 1 -- ambiguous
        | c >= '\x3250' && c <= '\x4DBF'   -> 2
        | c >= '\x4DC0' && c <= '\x4DFF'   -> 1
        | c >= '\x4E00' && c <= '\xA4C6'   -> 2
        | c >= '\xA4D0' && c <= '\xA95F'   -> 1
        | c >= '\xA960' && c <= '\xA97C'   -> 2
        | c >= '\xA980' && c <= '\xABF9'   -> 1
        | c >= '\xAC00' && c <= '\xD7FB'   -> 2
        | c >= '\xD800' && c <= '\xDFFF'   -> 1
        | c >= '\xE000' && c <= '\xF8FF'   -> 1 -- ambiguous
        | c >= '\xF900' && c <= '\xFAFF'   -> 2
        | c >= '\xFB00' && c <= '\xFDFD'   -> 1
        | c >= '\xFE00' && c <= '\xFE0F'   -> 1 -- ambiguous
        | c >= '\xFE10' && c <= '\xFE19'   -> 2
        | c >= '\xFE20' && c <= '\xFE26'   -> 1
        | c >= '\xFE30' && c <= '\xFE6B'   -> 2
        | c >= '\xFE70' && c <= '\xFEFF'   -> 1
        | c >= '\xFF01' && c <= '\xFF60'   -> 2
        | c >= '\xFF61' && c <= '\x16A38'  -> 1
        | c >= '\x1B000' && c <= '\x1B001' -> 2
        | c >= '\x1D000' && c <= '\x1F1FF' -> 1
        | c >= '\x1F200' && c <= '\x1F251' -> 2
        | c >= '\x1F300' && c <= '\x1F773' -> 1
        | c >= '\x20000' && c <= '\x3FFFD' -> 2
        | otherwise                        -> 1

-- | Get real length of string, taking into account combining and double-wide
-- characters.
realLength :: String -> Int
realLength = foldl' (+) 0 . map charWidth
