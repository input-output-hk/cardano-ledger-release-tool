{- | Deciding what to do about release tags, given where CHaP says a package
version was built from and where the tag currently points, locally and on
origin. Pure, so it can be unit tested; the git calls live in "Common.Git".
-}
module Release.Tags (
  TagState (..),
  TagAction (..),
  TagProblem (..),
  decideTag,
  parseRemoteTags,
  describeTagProblem,
) where

import Data.Either (partitionEithers)
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- | Where a tag points in one repository (local or remote).
data TagState
  = TagAbsent
  | -- | The full SHA of the commit the tag points to.
    TagAt String
  deriving (Eq, Show)

-- | What @release post@ should do about one package's tag.
data TagAction
  = -- | Already at the recorded commit, locally and on origin.
    TagSkip
  | -- | Missing everywhere: create it locally and push it.
    TagCreate
  | {- | Correct locally but missing on origin, e.g. created by an earlier
    @--apply@ run without @--push@: only push it.
    -}
    TagPushOnly
  | -- | Correct on origin but missing locally: fetch it.
    TagFetch
  deriving (Eq, Show)

-- | A tag that exists but points somewhere other than the commit CHaP recorded.
data TagProblem
  = LocalTagElsewhere String
  | RemoteTagElsewhere String
  deriving (Eq, Show)

{- | Decide what to do about a tag from the commit CHaP recorded, the local
tag and the tag on origin. A tag pointing elsewhere is never moved: every such
tag is reported, and the result is a 'Left' (never @Left []@).
-}
decideTag :: String -> TagState -> TagState -> Either [TagProblem] TagAction
decideTag wanted local remote =
  case elsewhere LocalTagElsewhere local ++ elsewhere RemoteTagElsewhere remote of
    [] -> Right $ case (local, remote) of
      (TagAbsent, TagAbsent) -> TagCreate
      (TagAbsent, TagAt _) -> TagFetch
      (TagAt _, TagAbsent) -> TagPushOnly
      (TagAt _, TagAt _) -> TagSkip
    problems -> Left problems
 where
  elsewhere problem (TagAt sha) | sha /= wanted = [problem sha]
  elsewhere _ _ = []

{- | Parse the output of @git ls-remote --tags origin@ into a map from tag name
to the commit it points to. An annotated tag appears twice: once with the SHA
of the tag object, and once as @<name>^{}@ with the peeled commit, which is
the one that matters here. A lightweight tag only has the plain line, whose
SHA is already the commit.
-}
parseRemoteTags :: String -> Map String String
parseRemoteTags out = Map.fromList peeled `Map.union` Map.fromList plain
 where
  (peeled, plain) = partitionEithers (mapMaybe classify (lines out))
  classify line = case words line of
    [sha, ref] -> do
      name <- stripPrefix "refs/tags/" ref
      pure $ case stripSuffix "^{}" name of
        Just tag -> Left (tag, sha)
        Nothing -> Right (name, sha)
    _ -> Nothing
  stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse

{- | Describe a 'TagProblem' for a tag, given the commit CHaP recorded and
whether origin has the tag at that commit, as one line that includes the fix.
-}
describeTagProblem :: String -> String -> Bool -> TagProblem -> Text
describeTagProblem tag wanted remoteAgrees problem = T.pack $ case problem of
  LocalTagElsewhere local
    | remoteAgrees ->
        tag
          <> ": local tag is at "
          <> short local
          <> " but CHaP recorded "
          <> short wanted
          <> " (origin agrees with CHaP); run 'git fetch --force --tags origin' and re-run"
    | otherwise ->
        tag
          <> ": local tag is at "
          <> short local
          <> " but CHaP recorded "
          <> short wanted
          <> "; run 'git tag -d "
          <> tag
          <> "' and re-run"
  RemoteTagElsewhere remote ->
    tag
      <> ": origin has the tag at "
      <> short remote
      <> " but CHaP recorded "
      <> short wanted
      <> "; fix origin by hand ('git push --delete origin "
      <> tag
      <> "', tell the team, re-run); never force-push"
 where
  short = take 7
