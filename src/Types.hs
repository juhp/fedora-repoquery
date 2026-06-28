{-# LANGUAGE OverloadedStrings #-}

module Types (
  Mirror(..),
  Natural,
  RepoSource(..),
  Verbosity(..),
  Release(..),
  eitherRelease,
  readRelease,
  elnVersion
  ) where

import Data.Char (isDigit)
import Data.List.Extra (lower, unsnoc)
import Numeric.Natural
import SimpleCmd (error', (+-+))

--import Distribution.Fedora.Repoquery

data Verbosity = Quiet | Normal | Verbose
  deriving Eq

data Mirror = DownloadFpo | DlFpo | CloudFront | Mirror String
  deriving Eq

-- FIXME: True for koji, make into type
data RepoSource = RepoSource Bool Mirror
  deriving Eq

data Release = EpelMinor Natural Natural
             | EPEL Natural
             | EPEL9Next
             | OldCentos Natural
             | Centos Natural
             | Fedora Natural
             | ELN
             | Rawhide
             | System
  deriving Eq

-- derived from fedora-releases Branch
-- | defined such that: EPELNext 9 < EPEL 10 < Fedora 41 < Rawhide
instance Ord Release where
  compare Rawhide Rawhide = EQ
  compare ELN ELN = EQ
  compare (Fedora m) (Fedora n) = compare m n
  compare (EPEL m) (EPEL n) = compare m n
  compare (EpelMinor m1 n1) (EpelMinor m2 n2) =
    case compare m1 m2 of
      LT -> LT
      GT -> GT
      EQ -> compare n1 n2
  compare EPEL9Next EPEL9Next = EQ
  compare (Centos m) (Centos n) = compare m n
  compare (OldCentos m) (OldCentos n) = compare m n
  compare Rawhide _ = GT
  compare _ Rawhide = LT
  compare ELN _ = GT
  compare _ ELN = LT
  compare (Fedora _) _ = GT
  compare _ (Fedora _) = LT
  compare (EPEL m) EPEL9Next = if m == 9 then LT else compare m 9
  compare EPEL9Next (EPEL n) = if n == 9 then GT else compare 9 n
  compare (EPEL m) (EpelMinor maj _) = if m >= maj then GT else LT
  compare (EpelMinor maj _) (EPEL n) = if maj <= n then LT else GT
  compare (EpelMinor _ _) EPEL9Next = GT
  compare EPEL9Next (EpelMinor _ _) = LT
  compare (Centos _) (OldCentos _) = GT
  compare (OldCentos _) (Centos _) = LT
  compare (Centos _) _ = LT
  compare _ (Centos _) = GT
  compare (OldCentos _) _ = LT
  compare _ (OldCentos _) = GT
  -- undefined really
  compare System _ = EQ
  compare _ System = EQ

-- FIXME determine via EPEL or Centos
elnVersion :: Natural
elnVersion = 11

-- FIXME error if short/release-like
-- | Read a Release name
eitherRelease :: String -> Either String Release
eitherRelease rel =
  case lower rel of
    "rawhide" -> Right Rawhide
    -- FIXME add proper parsing:
    "epel9-next" -> Right EPEL9Next
    -- disallow "epel9.4" etc
    ('e':'p':'e':'l':d:'.':_) | isDigit d -> error' $ "unknown release:" +-+ rel
    -- disallow "epel10." etc
    ['e','p','e','l',d,d','.'] | all isDigit [d,d'] -> error' $ "bad release:" +-+ rel
    ('e':'p':'e':'l':'1':d:'.':n@(_:_)) | all isDigit (d:n) ->
                                            Right $
                                            EpelMinor (read ['1',d]) $ read n
    ('e':'p':'e':'l':n@(_:_)) | all isDigit n -> Right $ EPEL $ read n
    ('c':'e':'n':'t':'o':'s':n@(_:_)) | all isDigit n ->
                                          Right $ OldCentos $ read n
    ('c':n@(_:_)) ->
      let ver =
            case unsnoc n of
              Just (v,'s') | all isDigit v ->
                if read v < (8 :: Natural)
                then error' $ "centos-stream" +-+ v +-+ "does not exist"
                else v
              _ -> n
      in if null ver
         then Left rel
         else
           if all isDigit ver
           then
             let v = read ver
             in Right $ (if v < 8 then OldCentos else Centos) v
           else Left rel
    "eln" -> Right ELN
    ('f':n@(_:_)) | all isDigit n -> let r = read n in Right (Fedora r)
    ns@(_:_) | all isDigit ns ->
               let r = read ns in
                 Right $
                 case compare r elnVersion of
                   LT -> (if r >= 8 then Centos else OldCentos) r
                   EQ -> ELN
                   GT -> Fedora r
    _ -> Left rel

readRelease :: String -> Maybe Release
readRelease s =
  case eitherRelease s of
    Right r -> Just r
    Left _ -> Nothing

instance Show Release where
  show Rawhide = "rawhide"
  show (Fedora n) = "f" ++ show n
  show (EpelMinor n m) = "epel" ++ show n ++ "." ++ show m
  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n
  show EPEL9Next = "epel9" ++ "-next"
  show ELN = "eln"
  show (Centos n) = 'c' : show n ++ "s"
  show (OldCentos n) = 'c' : show n
  show System = "system"
