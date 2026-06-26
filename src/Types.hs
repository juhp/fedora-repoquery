{-# LANGUAGE OverloadedStrings #-}

module Types (
  Mirror(..),
  Natural,
  RepoSource(..),
  Verbosity(..),
  Release(..),
  eitherRelease
  ) where

import Data.Char (isDigit)
import Data.List.Extra (lower, unsnoc)
import Numeric.Natural

--import Distribution.Fedora.Repoquery

data Verbosity = Quiet | Normal | Verbose
  deriving Eq

data Mirror = DownloadFpo | DlFpo | CloudFront | Mirror String
  deriving Eq

-- FIXME: True for koji make into type
data RepoSource = RepoSource Bool Mirror
  deriving Eq

-- FIXME deal with epel11.x
data Release = EpelMinor Natural Natural
             | EPEL Natural
             | EPEL9Next
             | OldCentos Natural
             | Centos Natural
             | Fedora Natural
             | ELN
             | Rawhide
             | System
  deriving (Eq, Ord)

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
    -- FIXME add epel11.x
    ('e':'p':'e':'l':'1':'0':'.':n@(_:_)) | all isDigit n ->
                                              Right $ EpelMinor 10 $ read n
    ('e':'p':'e':'l':n@(_:_)) | all isDigit n -> Right $ EPEL $ read n
    ('c':'e':'n':'t':'o':'s':n@(_:_)) | all isDigit n ->
                                          Right $ OldCentos $ read n
    ('c':n@(_:_)) ->
      let ver =
            case unsnoc n of
              Just (v,'s') | all isDigit v ->
                if read v < (8 :: Natural)
                then error $ "centos-stream " ++ v ++ " does not exist"
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
