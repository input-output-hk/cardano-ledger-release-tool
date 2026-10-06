{-# LANGUAGE OverloadedStrings #-}

module Test.Release.Spec where

import Data.Text (Text)
import Release.Meta
import Release.Tags
import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "parseChapMeta" $ do
    specify "canonical file" $
      parseChapMeta (T.unlines canonical) `shouldBe` Right canonicalMeta
    specify "revisions and deprecations are ignored" $
      parseChapMeta
        ( T.unlines $
            canonical
              ++ [ ""
                 , "[[revisions]]"
                 , "number = 1"
                 , "timestamp = 2023-04-01T12:00:00Z"
                 , ""
                 , "[[revisions]]"
                 , "number = 2"
                 , "timestamp = 2023-05-01T12:00:00Z"
                 , ""
                 , "[[deprecations]]"
                 , "timestamp = 2023-06-01T12:00:00Z"
                 , "deprecated = true"
                 ]
        )
        `shouldBe` Right canonicalMeta
    specify "abbreviated rev" $
      srcRev . metaSource <$> parseChapMeta (withGithub "IntersectMBO/cardano-ledger" "f649f9751")
        `shouldBe` Right "f649f9751"
    specify "repo is kept verbatim" $
      srcRepo . metaSource <$> parseChapMeta (withGithub "intersectmbo/cardano-ledger" fullRev)
        `shouldBe` Right "intersectmbo/cardano-ledger"
    specify "no subdir" $
      metaSubdir <$> parseChapMeta (T.unlines (take 2 canonical))
        `shouldBe` Right Nothing
    specify "double-quoted subdir" $
      metaSubdir <$> parseChapMeta (T.unlines (take 2 canonical ++ ["subdir = \"libs/x\""]))
        `shouldBe` Right (Just "libs/x")
    specify "rev is lowercased" $
      srcRev . metaSource <$> parseChapMeta (withGithub "IntersectMBO/cardano-ledger" (T.toUpper fullRev))
        `shouldBe` Right fullRev
    specify "quoted timestamp is not decoded" $
      parseChapMeta (T.unlines ("timestamp = '2022-03-29T06:19:50+00:00'" : drop 1 canonical))
        `shouldBe` Right canonicalMeta
    specify "tarball source" $
      parseChapMeta
        ( T.unlines
            [ "timestamp = 2023-02-10T11:32:54Z"
            , "url = \"file:tarballs/pkg-a.tar.gz\""
            ]
        )
        `shouldBe` Right (ChapMeta (UrlSource "file:tarballs/pkg-a.tar.gz") Nothing)
    specify "empty file" $
      parseChapMeta "" `shouldSatisfy` isLeft
    specify "github without rev" $
      parseChapMeta "github = { repo = \"a/b\" }\n" `shouldSatisfy` failsMentioning "rev"
    specify "rev that is not a commit id" $ do
      parseChapMeta (withGithub "a/b" "HEAD") `shouldSatisfy` failsMentioning "HEAD"
      parseChapMeta (withGithub "a/b" "abc") `shouldSatisfy` failsMentioning "abc"
    specify "malformed toml" $
      parseChapMeta "github = { repo = \n" `shouldSatisfy` isLeft
    specify "CRLF line endings" $
      parseChapMeta (T.concat (map (<> "\r\n") canonical)) `shouldBe` Right canonicalMeta

  describe "decideTag" $ do
    specify "absent everywhere: create" $
      decideTag w TagAbsent TagAbsent `shouldBe` Right TagCreate
    specify "only origin has it, at the recorded commit: fetch" $
      decideTag w TagAbsent (TagAt w) `shouldBe` Right TagFetch
    specify "only origin has it, elsewhere" $
      decideTag w TagAbsent (TagAt x) `shouldBe` Left [RemoteTagElsewhere x]
    specify "only local has it, at the recorded commit: push" $
      decideTag w (TagAt w) TagAbsent `shouldBe` Right TagPushOnly
    specify "both at the recorded commit: skip" $
      decideTag w (TagAt w) (TagAt w) `shouldBe` Right TagSkip
    specify "local right, origin elsewhere" $
      decideTag w (TagAt w) (TagAt x) `shouldBe` Left [RemoteTagElsewhere x]
    specify "local elsewhere, origin absent" $
      decideTag w (TagAt x) TagAbsent `shouldBe` Left [LocalTagElsewhere x]
    specify "local elsewhere, origin right" $
      decideTag w (TagAt x) (TagAt w) `shouldBe` Left [LocalTagElsewhere x]
    specify "both elsewhere" $
      decideTag w (TagAt x) (TagAt y) `shouldBe` Left [LocalTagElsewhere x, RemoteTagElsewhere y]

  describe "parseRemoteTags" $ do
    specify "annotated tag: the peeled line wins" $
      parseRemoteTags (unlines [x <> "\trefs/tags/pkg-1.0.0", w <> "\trefs/tags/pkg-1.0.0^{}"])
        `shouldBe` Map.fromList [("pkg-1.0.0", w)]
    specify "annotated tag: peeled line first" $
      parseRemoteTags (unlines [w <> "\trefs/tags/pkg-1.0.0^{}", x <> "\trefs/tags/pkg-1.0.0"])
        `shouldBe` Map.fromList [("pkg-1.0.0", w)]
    specify "lightweight tag" $
      parseRemoteTags (w <> "\trefs/tags/pkg-1.0.0\n")
        `shouldBe` Map.fromList [("pkg-1.0.0", w)]
    specify "other refs are ignored" $
      parseRemoteTags (unlines [x <> "\trefs/heads/master", w <> "\trefs/tags/pkg-1.0.0"])
        `shouldBe` Map.fromList [("pkg-1.0.0", w)]
    specify "empty output" $
      parseRemoteTags "" `shouldBe` Map.empty

  describe "describeTagProblem" $ do
    specify "local tag elsewhere, origin agrees with CHaP" $
      describeTagProblem "pkg-1.0.0" w True (LocalTagElsewhere x)
        `shouldBe` "pkg-1.0.0: local tag is at 1111111 but CHaP recorded 0000000 (origin agrees with CHaP); run 'git fetch --force --tags origin' and re-run"
    specify "local tag elsewhere, origin does not agree" $
      describeTagProblem "pkg-1.0.0" w False (LocalTagElsewhere x)
        `shouldBe` "pkg-1.0.0: local tag is at 1111111 but CHaP recorded 0000000; run 'git tag -d pkg-1.0.0' and re-run"
    specify "origin tag elsewhere" $
      describeTagProblem "pkg-1.0.0" w False (RemoteTagElsewhere y)
        `shouldBe` "pkg-1.0.0: origin has the tag at 2222222 but CHaP recorded 0000000; fix origin by hand ('git push --delete origin pkg-1.0.0', tell the team, re-run); never force-push"
 where
  fullRev = "f649f9751074d2ab3de033fc3912f29c9862c1f5"
  canonical =
    [ "timestamp = 2023-02-10T11:32:54Z"
    , "github = { repo = \"IntersectMBO/cardano-ledger\", rev = \"" <> fullRev <> "\" }"
    , "subdir = 'libs/cardano-ledger-binary'"
    ]
  canonicalMeta =
    ChapMeta (GitHubSource "IntersectMBO/cardano-ledger" fullRev) (Just "libs/cardano-ledger-binary")
  withGithub repo rev =
    "github = { repo = \"" <> repo <> "\", rev = \"" <> rev <> "\" }\n"

  isLeft :: Either Text ChapMeta -> Bool
  isLeft = either (const True) (const False)
  failsMentioning :: Text -> Either Text ChapMeta -> Bool
  failsMentioning needle = either (T.isInfixOf needle) (const False)

  w, x, y :: String
  w = replicate 40 '0'
  x = replicate 40 '1'
  y = replicate 40 '2'
