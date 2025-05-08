# fedora-repoquery

A dnf wrapper to repoquery different Fedora releases,
caching repodata separately per release.
It also supports EPEL, Centos Stream and ELN repos.

## Usage
Usage examples:

`$ fedora-repoquery rawhide firefox`

```
firefox-138.0.1-1.fc43.x86_64 (fedora-rawhide)
```

`$ fedora-repoquery 42 --requires filesystem`

```
setup
```

`$ fedora-repoquery epel9 ghc`

```
ghc-8.10.7-116.el9.x86_64 (epel9)
```

`$ fedora-repoquery c10 bash`

```
bash-5.2.26-6.el10.x86_64 (c10s-BaseOS)
```

`$ fedora-repoquery eln kernel`

```
kernel-6.15.0-0.rc5.250507g0d8d44db295c.45.eln148.x86_64 (eln-BaseOS)
```

Without a release argument, the system yum repo configuration is used:

`$ fedora-repoquery pandoc`

```
pandoc-3.6.4-35.fc43.x86_64 (rawhide)
```
but then currently you have to insert `--` before a query option:

`$ fedora-repoquery -- --whatrequires pandoc`

```
R-reprex-2.1.1-1.fc43.noarch (rawhide)
R-rmarkdown-2.24-8.fc43.noarch (rawhide)
pandoc-pdf-3.6.4-35.fc43.x86_64 (rawhide)
python3-pypandoc-1.15-3.fc42.noarch (rawhide)
```

Use the --time (-T) option to display repo timestamps:
```
$ fedora-repoquery -T 43 fedrq
2025-05-07 15:57:29 +08 <https://mirror.freedif.org/fedora/fedora/linux/development/rawhide/>

fedrq-1.5.0-1.fc43.noarch (fedora-rawhide)
```

Repo timestamp(s) are output when there are no query args
after the release version:

`$ fedora-repoquery 42`

```
2025-04-15 01:43:43 +08 <https://mirror.freedif.org/fedora/fedora/linux/releases/42/>
2025-05-08 12:18:02 +08 <https://mirror.freedif.org/fedora/fedora/linux/updates/42/>
```

One can also query multiple releases (or arch's):

`$ fedora-repoquery 41 42 python3 | grep x86_64`

```
python3-3.13.0-1.fc41.x86_64 (f41)
python3-3.13.3-2.fc41.x86_64 (f41-updates)
python3-3.13.2-2.fc42.x86_64 (f42)
python3-3.13.3-2.fc42.x86_64 (f42-updates)
```


## Help
`$ fedora-repoquery --version`

```
0.7.2
```

`$ fedora-repoquery --help`

```
fedora-repoquery tool for querying Fedora repos for packages.

Usage: fedora-repoquery [--version] [-4|--dnf4] [(-q|--quiet) | (-v|--verbose)]
                        [--dynamic] [-T|--time] [-K|--koji]
                        [(-m|--mirror URL) | (-D|--dl)]
                        [(-s|--source) | (-A|--all-archs) | [-a|--arch ARCH]]
                        [-t|--testing] [-d|--debug]
                        ((-z|--cache-size) | (-e|--cache-clean-empty) |
                          (-l|--list) |
                          [RELEASE|--]... [REPOQUERY_OPTS]... [PACKAGE]...)

  where RELEASE is {fN or N (fedora), 'rawhide', epelN, epelN-next, cN (centos
  stream), 'eln'}, with N the release version number.
  https://github.com/juhp/fedora-repoquery#readme

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
  -D,--dl                  Use dl.fp.o
  -s,--source              Query source repos
  -A,--all-archs           Query all (64 bit) arch repos
  -a,--arch ARCH           Specify arch [default: x86_64]
  -t,--testing             Fedora updates-testing
  -d,--debug               Show some debug output
  -z,--cache-size          Show total dnf repo metadata cache disksize
  -e,--cache-clean-empty   Remove empty dnf caches
  -l,--list                List Fedora versions
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
