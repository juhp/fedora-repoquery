{-# LANGUAGE OverloadedStrings #-}

module Types (
  Arch(..),
  readArch,
  showArch,
  Mirror(..),
  RepoSource(..),
  showRepoSource,
  Channel(..),
  channel,
  Verbosity(..)
  ) where

import Data.List.Extra

--import Distribution.Fedora.Repoquery

data Verbosity = Quiet | Normal | Verbose
  deriving Eq

data Arch = Source
          | X86_64
          | AARCH64
          | ARMV7HL
          | S390X
          | PPC64LE
          | I386
  deriving Eq

readArch :: String -> Either String Arch
readArch s =
  case lower s of
    "source" -> Right Source
    "x86_64" -> Right X86_64
    "aarch64" -> Right AARCH64
    "armv7hl" -> Right ARMV7HL
    "s390x" -> Right S390X
    "ppc64le" -> Right PPC64LE
    "i386" -> Right I386
    _ -> Left $ "unknown arch: " ++ s

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

data RepoSource = RepoFedora Mirror
                | RepoKoji
                | RepoCentosStream Channel
  deriving Eq

showRepoSource :: RepoSource -> String
showRepoSource (RepoFedora _) = "Fedora"
showRepoSource RepoKoji = "Koji"
showRepoSource (RepoCentosStream _) = "Centos Stream"

data Channel = Devel | Test | Prod
  deriving Eq

channel :: Channel -> String
channel Devel = "development"
channel Test = "test"
channel Prod = "production"

instance Show Channel where
  show Devel = "devel"
  show Test = "test"
  show Prod = "prod"
