{-# LANGUAGE OverloadedStrings #-}

module Types (
  Arch(..),
  allArchs,
  eitherArch,
  readArch,
  showArch,
  Mirror(..),
  Natural,
  RepoSource(..),
  Channel(..),
  channel,
  Verbosity(..),
  Release(..),
  readRelease
  ) where

import Data.Char (isDigit)
import Data.List.Extra
import Numeric.Natural
import SimpleCmd (error')

--import Distribution.Fedora.Repoquery

data Verbosity = Quiet | Normal | Verbose
  deriving Eq

data Arch = Source
          | X86_64
          | AARCH64
          | ARMV7HL
          | PPC64LE
          | S390X
          | I386
  deriving Eq

allArchs :: [Arch]
allArchs = [X86_64, AARCH64, PPC64LE, S390X]

eitherArch :: String -> Either String Arch
eitherArch s =
  case lower s of
    "source" -> Right Source
    "x86_64" -> Right X86_64
    "aarch64" -> Right AARCH64
    "armv7hl" -> Right ARMV7HL
    "s390x" -> Right S390X
    "ppc64le" -> Right PPC64LE
    "i386" -> Right I386
    _ -> Left $ "unknown arch: " ++ s

readArch :: String -> Arch
readArch =
  either error' id . eitherArch

showArch :: Arch -> String
showArch Source = "source"
showArch X86_64 = "x86_64"
showArch AARCH64 = "aarch64"
showArch ARMV7HL = "armv7hl"
showArch S390X = "s390x"
showArch PPC64LE = "ppc64le"
showArch I386 = "i386"

data Mirror = DownloadFpo | DlFpo | Mirror String
  deriving Eq

-- True for koji
data RepoSource = RepoSource Bool Channel Mirror
  deriving Eq

data Channel = ChanDevel | ChanTest | ChanProd
  deriving Eq

channel :: Channel -> String
channel ChanDevel = "development"
channel ChanTest = "test"
channel ChanProd = "production"

instance Show Channel where
  show ChanDevel = "devel"
  show ChanTest = "test"
  show ChanProd = "prod"

data Release = EPEL Natural | EPELNext Natural | Centos Natural | Fedora Natural | ELN | Rawhide
  deriving (Eq, Ord)

elnVersion :: Natural
elnVersion = 11

-- | Read a Release name, otherwise return an error message
eitherRelease :: String -> Either String Release
eitherRelease "rawhide" = Right Rawhide
-- FIXME add proper parsing:
eitherRelease "epel8-next" = Right $ EPELNext 8
eitherRelease "epel9-next" = Right $ EPELNext 9
eitherRelease ('e':'p':'e':'l':n) | all isDigit n = let br = EPEL (read n) in Right br
eitherRelease ('e':'l':n) | all isDigit n = let r = read n in Right (EPEL r)
eitherRelease ('c':n) | all isDigit n = let r = read n in Right (Centos r)
eitherRelease ('C':n) | all isDigit n = let r = read n in Right (Centos r)
eitherRelease "eln" = Right ELN
eitherRelease ('f':ns) | all isDigit ns = let r = read ns in Right (Fedora r)
eitherRelease ns | all isDigit ns = let r = read ns in
                     Right $
                     case compare r elnVersion of
                       LT -> Centos r
                       EQ -> ELN
                       GT -> Fedora r
eitherRelease cs = Left $ cs ++ " is not a known os release"

-- | Read a Fedora Release name
readRelease :: String -> Maybe Release
readRelease bs =
  case eitherRelease bs of
    Left _ -> Nothing
    Right br -> Just br

instance Show Release where
  show Rawhide = "rawhide"
  show (Fedora n) = "f" ++ show n
  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n
  show (EPELNext n) = "epel" ++ show n ++ "-next"
  show ELN = "eln"
  show (Centos n) = 'c' : show n ++ "s"
