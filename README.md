# fedora-repoquery

A dnf wrapper to repoquery different Fedora releases,
caching repodata separately per release.
It also supports EPEL and Centos Stream repos.

## Usage
Usage examples:

`$ fedora-repoquery rawhide firefox`

```
firefox-130.0-1.fc42.x86_64 (fedora-rawhide)
```

`$ fedora-repoquery 40 --requires filesystem`

```
setup
```

`$ fedora-repoquery epel9 ghc`

```
ghc-8.10.7-116.el9.x86_64 (epel9)
```

`$ fedora-repoquery c10 bash`

```
bash-5.2.26-4.el10.x86_64 (c10s-BaseOS)
```

`$ fedora-repoquery eln kernel`

```
kernel-6.11.0-0.rc6.20240906gitb831f83e40a2.53.eln142.x86_64 (eln-BaseOS)
```

Without a release argument, the system yum repo configuration is used:

`$ fedora-repoquery pandoc`

```
pandoc-3.1.11.1-32.fc41.x86_64 (rawhide)
```
but then currently you have to insert `--` before a query option:

`$ fedora-repoquery -- --whatrequires pandoc`

```
R-reprex-2.0.2-12.fc41.noarch (rawhide)
R-rmarkdown-2.24-5.fc41.noarch (rawhide)
pandoc-pdf-3.1.11.1-32.fc41.x86_64 (rawhide)
python3-nb2plots-0.7.2-5.fc41.noarch (rawhide)
python3-pypandoc-1.13-6.fc41.noarch (rawhide)
rstudio-common-2024.04.2+764-3.fc41.x86_64 (rawhide)
```

Use the --time (-T) option to display repo timestamps:
```
$ fedora-repoquery -T 41 fedrq
2024-07-16 19:45:44 +08 <https://mirror.freedif.org/fedora/fedora/linux/development/rawhide>

fedrq-1.1.0-3.fc41.noarch (fedora-rawhide)
```

Repo timestamp(s) are output when there are no query args
after the release version:

`$ fedora-repoquery 40`

```
2024-04-20 02:22:34 +08 <https://mirror.freedif.org/fedora/fedora/linux/releases/40/>
2024-09-07 09:37:56 +08 <https://mirror.freedif.org/fedora/fedora/linux/updates/40/>
```

One can also query multiple releases (or arch's):

`$ fedora-repoquery 40 41 python3 | grep x86_64`

```
python3-3.12.2-2.fc40.x86_64 (f40)
python3-3.12.5-2.fc40.x86_64 (f40-updates)
python3-3.13.0~rc1-2.fc41.x86_64 (f41-development)
python3-3.13.0~rc1-3.fc41.x86_64 (f41-updates-testing)
```


## Help
`$ fedora-repoquery --version`

```
0.7.1
```

`$ fedora-repoquery --help`

```
fedora-repoquery tool for querying Fedora repos for packages.

Usage: fedora-repoquery [--version] [-4|--dnf4] [(-q|--quiet) | (-v|--verbose)]
                        [--dynamic] [-T|--time] [-K|--koji]
                        [--devel-channel | --test-channel]
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
  --devel-channel          Use eln development compose
  --test-channel           Use eln test compose [default: production]
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
fedora-repoquery is available in Fedora and EPEL 9:
<https://src.fedoraproject.org/rpms/fedora-repoquery>.

## Building from source
Use `stack install fedora-repoquery` or `cabal install fedora-repoquery`
to build the latest release.

To build from git: `stack install` or `cabal install` or `cabal-rpm install`.

## Contributing
fedora-repoquery is distributed under the GPL license version 3 or later.

<https://github.com/juhp/fedora-repoquery>
