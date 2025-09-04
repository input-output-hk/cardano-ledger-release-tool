{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Changelog where

import CMark
import Control.Monad ((<=<), (>=>))
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Text.Lazy (Text, unpack)
import Data.Version (Version, parseVersion, showVersion)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Pretty.Simple (pShow)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- CMark helper functions

nodePos :: Node -> Maybe PosInfo
nodePos (Node mp _ _) = mp

nodeType :: Node -> NodeType
nodeType (Node _ ty _) = ty

nodeChildren :: Node -> [Node]
nodeChildren (Node _ _ ns) = ns

nodeText :: Node -> Text
nodeText (Node _ typ children) = mconcat $ implicitText : map nodeText children
 where
  implicitText = TL.fromStrict $ case typ of
    CODE t -> t
    CODE_BLOCK _ t -> t
    HTML_BLOCK t -> t
    HTML_INLINE t -> t
    TEXT t -> t
    _ -> mempty

type Markdown = [Node]

-- Grouping into sections

data Section = Section
  { sectionLevel :: Int
  , sectionTitle :: Markdown
  , sectionPreamble :: Markdown
  , sectionSubsections :: [Section]
  }
  deriving (Eq, Ord, Show)

buildSections :: Node -> Except Text (Markdown, [Section])
buildSections (Node _ DOCUMENT docNodes) = pure $ foldr go mempty docNodes
 where
  go (Node _ (HEADING level) titleNodes) (ns, ss) =
    let (children, others) = span ((level <) . sectionLevel) ss
     in ([], Section level titleNodes ns children : others)
  go n (ns, ss) =
    (n : ns, ss)
buildSections (Node mPos typ _) = throwError $ "Unexpected top-level node type (" <> pShow typ <> ") at " <> pShow mPos

unbuildSections :: (Markdown, [Section]) -> Node
unbuildSections (md, ss) =
  Node Nothing DOCUMENT $ md <> concatMap go ss
 where
  go :: Section -> Markdown
  go Section {..} =
    Node Nothing (HEADING sectionLevel) sectionTitle
      : sectionPreamble <> concatMap go sectionSubsections

-- Changelogs

data Changelog = Changelog
  { changelogTitle :: Markdown
  , changelogPreamble :: [Paragraph]
  , changelogReleases :: [Release]
  }
  deriving (Eq, Ord, Show)

data Release = Release
  { releaseNumber :: Version
  , releasePreamble :: [Paragraph]
  , releaseEntries :: [Entry]
  , releaseSublibs :: [Sublib]
  }
  deriving (Eq, Ord, Show)

data Sublib = Sublib
  { sublibName :: Markdown
  , sublibEntries :: [Entry]
  }
  deriving (Eq, Ord, Show)

newtype Paragraph = Paragraph
  { unParagraph :: Markdown
  }
  deriving (Eq, Ord, Show)

newtype Entry = Entry
  { unEntry :: Markdown
  }
  deriving (Eq, Ord, Show)

parseChangelog :: Text -> Either Text Changelog
parseChangelog = runExcept . (makeChangeLog <=< buildSections . commonmarkToNode [] . TL.toStrict)

renderChangelog :: Text -> Changelog -> Text
renderChangelog bullets = fixMarkdownStyle bullets . TL.fromStrict . nodeToCommonmark [] Nothing . unbuildSections . unmakeChangelog

makeChangeLog :: (Markdown, [Section]) -> Except Text Changelog
makeChangeLog ([], [Section 1 title paragraphs sections]) =
  Changelog title <$> makeParagraphs paragraphs <*> traverse makeRelease sections
makeChangeLog unexpected = throwError $ "Unexpected Changelog input: " <> pShow unexpected

unmakeChangelog :: Changelog -> (Markdown, [Section])
unmakeChangelog Changelog {..} =
  ([], [Section 1 changelogTitle (unmakeParagraphs changelogPreamble) $ map unmakeRelease changelogReleases])

makeRelease :: Section -> Except Text Release
makeRelease (Section 2 title markdown subsections) =
  Release <$> makeVersion title <*> makeParagraphs paragraphs <*> makeEntries entries <*> traverse makeSublib subsections
 where
  (paragraphs, entries) = span ((PARAGRAPH ==) . nodeType) markdown
makeRelease unexpected = throwError $ "Unexpected Release parse result: " <> pShow unexpected

unmakeRelease :: Release -> Section
unmakeRelease Release {..} =
  Section
    2
    (unmakeVersion releaseNumber)
    (unmakeParagraphs releasePreamble <> unmakeEntries releaseEntries)
    (map unmakeSublib releaseSublibs)

makeVersion :: Markdown -> Except Text Version
makeVersion = parseVersion' . mconcat . map nodeText
 where
  parseVersion' :: Text -> Except Text Version
  parseVersion' t = case sortOn (length . snd) . readP_to_S parseVersion . unpack $ t of
    (v, _) : _ -> pure v
    unexpected -> throwError $ "Unexpected Version parse result: " <> pShow unexpected

unmakeVersion :: Version -> Markdown
unmakeVersion v = [Node Nothing (TEXT $ T.pack . showVersion $ v) []]

makeSublib :: Section -> Except Text Sublib
makeSublib (Section 3 title markdown []) = Sublib title <$> makeEntries markdown
makeSublib unexpected = throwError $ "Unexpected Sublib input: " <> pShow unexpected

unmakeSublib :: Sublib -> Section
unmakeSublib Sublib {..} = Section 3 sublibName (unmakeEntries sublibEntries) []

makeParagraphs :: Markdown -> Except Text [Paragraph]
makeParagraphs markdown
  | all ((PARAGRAPH ==) . nodeType) markdown = pure $ map (Paragraph . nodeChildren) markdown
makeParagraphs unexpected = throwError $ "Unexpected Paragraphs input: " <> pShow unexpected

unmakeParagraphs :: [Paragraph] -> Markdown
unmakeParagraphs = map (Node Nothing PARAGRAPH . unParagraph)

makeEntries :: Markdown -> Except Text [Entry]
makeEntries [Node _ (LIST _) entries]
  | all ((ITEM ==) . nodeType) entries = pure $ map (Entry . nodeChildren) entries
makeEntries [] = pure []
makeEntries unexpected = throwError $ "Unexpected Entries input: " <> pShow unexpected

unmakeEntries :: [Entry] -> Markdown
unmakeEntries [] = []
unmakeEntries es = [Node Nothing (LIST listAttrs) $ map (Node Nothing ITEM . unEntry) es]
 where
  listAttrs =
    ListAttributes
      { listType = BULLET_LIST
      , listTight = True
      , listStart = 0
      , listDelim = PERIOD_DELIM
      }

-- Modify CMark output to match our preferred style (and fix some bugs in it)

fixMarkdownStyle :: Text -> Text -> Text
fixMarkdownStyle bullets = TL.unlines . fixup . TL.lines
 where
  fixup =
    concatMap . foldr (>=>) pure $
      [ fixIndent
      , fixBullets
      , fixNumbered
      , fixEscapes
      , fixEmptyListItems
      , fixHtmlComments
      ]
  -- We prefer flush-left top-level lists and two-space indentation
  fixIndent l =
    let
      (spaces, rest) = TL.span (== ' ') l
      level = TL.length spaces `div` 4
     in
      pure $ TL.replicate level "  " <> rest
  -- We want different bullet characters on different list levels
  fixBullets l =
    let
      (spaces, rest) = TL.span (== ' ') l
      level = TL.length spaces `div` 2
      bullet = TL.index bullets (level `mod` TL.length bullets)
      (lead, trail) = TL.splitAt 2 rest
      lead' = if lead == "- " then TL.cons bullet " " else lead
     in
      pure $ spaces <> lead' <> trail
  -- We want a single space after the number in numbered list items
  fixNumbered l =
    let
      (spaces, rest) = TL.span (== ' ') l
      (lead, trail) = TL.break (== ' ') rest
      isNumbered = case TL.unsnoc lead of
        Just (pfx, '.') -> TL.all isDigit pfx
        _ -> False
      trail' = if isNumbered then " " <> TL.stripStart trail else trail
     in
      pure $ spaces <> lead <> trail'
  -- See https://github.com/commonmark/cmark/issues/131
  fixEscapes = pure . TL.replace "\\#" "#" . TL.replace "\\>" ">"
  -- See https://github.com/commonmark/cmark/issues/583
  fixEmptyListItems l = if l == "* " then ["*", ""] else [l]
  -- See https://github.com/commonmark/cmark/pull/372
  fixHtmlComments l = [l | TL.strip l /= "<!-- end list -->"]
