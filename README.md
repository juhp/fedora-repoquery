# fedora-repoquery

A dnf wrapper to repoquery different Fedora releases,
caching repodata separately per release.
It also supports EPEL, Centos Stream and ELN repos.

## Usage
Usage examples:

`$ fedora-repoquery rawhide firefox`

```
firefox-148.0-1.fc45.x86_64 (fedora-rawhide)
```
(note it is shorter to write `fedora-repoquery 45`).

`$ fedora-repoquery 43 --requires filesystem`

```
group(mail)
setup
```

`$ fedora-repoquery epel10 ghc`

```
ghc-9.6.6-146.el10_0.x86_64 (epel10)
```

`$ fedora-repoquery c10 bash`

```
bash-5.2.26-6.el10.x86_64 (c10s-BaseOS)
```

`$ fedora-repoquery eln kernel`

```
kernel-7.0.0-0.rc3.260312g80234b5ab240.32.eln155.x86_64 (eln-BaseOS)
```

Without a release argument, the system yum repo configuration is used:

`$ fedora-repoquery pandoc-cli`

```
pandoc-cli-3.7.0.2-40.fc44.x86_64 (rawhide)
```

`$ fedora-repoquery --whatrequires pandoc`

```
pandoc-pdf-3.7.0.2-40.fc44.x86_64 (rawhide)
python3-pypandoc-1.16.2-1.fc44.noarch (rawhide)
```

Use the --time (-T) option to display repo timestamps:
```
$ fedora-repoquery -T 43 fedrq
2025-10-26 01:59:02 +08 <https://mirror.freedif.org/fedora/fedora/linux/releases/43/>
2026-03-12 09:53:14 +08 <https://mirror.freedif.org/fedora/fedora/linux/updates/43/>

fedrq-1.5.0-5.fc43.noarch (f43)
```

Repo timestamp(s) are output when there are no query args
after the release version:

`$ fedora-repoquery 44`

```
2026-03-13 19:08:04 +08 <https://mirror.freedif.org/fedora/fedora/linux/development/44/>
2026-03-13 09:38:02 +08 <https://mirror.freedif.org/fedora/fedora/linux/updates/testing/44/>
```

One can also query multiple releases (or arch's):

`$ fedora-repoquery 43 44 python3.x86_64`

```
python3-3.14.0-1.fc43.x86_64 (f43)
python3-3.14.3-1.fc43.x86_64 (f43-updates)
python3-3.14.3-1.fc44.x86_64 (f44-development)
```

There is an option to use the CloudFront cache:

`$ fedora-repoquery --cf 44 rust`

```
rust-1.93.1-1.fc44.x86_64 (f44-development-cf)
rust-1.94.0-1.fc44.x86_64 (f44-updates-testing-cf)
```
which may be faster than dl.fp.o and fresher than other mirrors sometimes.


## Help
`$ fedora-repoquery --version`

```
0.8.1
```

`$ fedora-repoquery --help`

```
fedora-repoquery tool for querying Fedora repos for packages.

Usage: fedora-repoquery [--version] [-4|--dnf4] [(-q|--quiet) | (-v|--verbose)]
                        [--dynamic] [-T|--time] [-K|--koji]
                        [(-m|--mirror URL) | --cf | (-d|--dl)] [-F|--all-fedora]
                        [(-s|--source) | (-A|--all-archs) |
                          [-a|--repo-arch ARCH]] [-t|--testing] [-D|--debug]
                        [(-z|--cache-size) | (-e|--cache-clean-empty) |
                          --list-releases | --help-dnf |
                          [--advisories ARG | --advisory-severities ARG |
                            --arch ARG | --available | --bugfix | --bzs ARG |
                            --cves ARG | --disable-modular-filtering |
                            --duplicates | --enhancement | --exactdeps |
                            --extras | (-f|--file ARG) | --installed |
                            --installed-from-repo ARG | --installonly |
                            --latest-limit ARG | --leaves | --newpackage |
                            --providers-of ARG | --recent | --recursive |
                            --security | --srpm | --unneeded | --upgrades |
                            --userinstalled | --whatconflicts ARG |
                            (--wd|--whatdepends ARG) | --whatenhances ARG |
                            --whatobsoletes ARG | (--whatprovides|--wp ARG) |
                            --whatrecommends ARG | (--whatrequires|--wr ARG) |
                            --whatsuggests ARG | --whatsupplements ARG |
                            --conflicts | --depends | --enhances | --files |
                            (-l|--list) | --obsoletes | (-p|--provides) |
                            --recommends | (-r|--requires) | --requires-pre |
                            --sourcerpm | --suggests | --supplements |
                            --location | (-i|--info) |
                            (--changelog|--changelogs) | --querytags |
                            (--qf|--queryformat ARG)]
                          [[RELEASE]... [REPOQUERY_OPTS]... [PKGSPECIFIER]...]]

  where RELEASE is {fN or N (fedora), 'rawhide', epelN, epelN-next, cN (centos
  stream), 'eln'}, with N the release version number.
  <https://github.com/juhp/fedora-repoquery#readme>

Available options:
  -h,--help                Show this help text
  --version                Show version
  -4,--dnf4                Use dnf4 instead of dnf5 (if available)
  -q,--quiet               Avoid output to stderr
  -v,--verbose             Show stderr from dnf repoquery
  --dynamic                Redirect each HTTP through mirror
  -T,--time                Show time-stamp of repos
  -K,--koji                Use Koji buildroot
  -m,--mirror URL          Fedora mirror [default:
                           https://download.fedoraproject.org/pub]
  --cf                     Use CloudFront cache
  -d,--dl                  Use dl.fp.o
  -F,--all-fedora          Query all Fedora releases
  -s,--source              Query source repos
  -A,--all-archs           Query all (64 bit) arch repos
  -a,--repo-arch ARCH      Specify repo arch [default: x86_64]
  -t,--testing             Fedora updates-testing
  -D,--debug               Show some debug output
  -z,--cache-size          Show total dnf repo metadata cache disksize
  -e,--cache-clean-empty   Remove empty dnf caches
  --list-releases          List Fedora versions
  --help-dnf               Show dnf --help
```
The default arch is the system arch.

## Installation
fedora-repoquery is available in Fedora and EPEL:
<https://src.fedoraproject.org/rpms/fedora-repoquery>.

## Building from source
Use `stack install fedora-repoquery` or `cabal install fedora-repoquery`
to build the latest release.

To build from git: `stack install` or `cabal install` or `cabal-rpm install`.

## Contributing
fedora-repoquery is distributed under the GPL license version 3 or later.

<https://github.com/juhp/fedora-repoquery>
