# fedora-repoquery

A work-in-progress wrapper for dnf repoquery,
which caches repodata separately per release.

## Usage
Usage examples:

- `fdrq rawhide firefox`

- `fdrq 39 --requires podman`

- `fdrq epel9 ghc`

- `fdrq c9 bash`

- `fdrq eln kernel`

etc

`$ fdrq --version`
```
0.3.1
```
`$ fdrq --help`
```
fedora-repoquery tool for querying Fedora repos for packages.

Usage: fdrq [--version] [(-q|--quiet) | (-v|--verbose)] [-K|--koji] 
            [--devel-channel | --test-channel] [(-m|--mirror URL) | (-D|--dl)] 
            [(-s|--source) | (-A|--all-archs) | [-a|--arch ARCH]] [-t|--testing]
            [-d|--debug] 
            ((-z|--cache-size) | (-e|--cache-clean-empty) | (-l|--list) | 
              RELEASE [[REPOQUERY_OPTS] [PACKAGE]...])

  where RELEASE is {fN or N (fedora), 'rawhide', epelN, epelN-next, cN (centos
  stream), 'eln'}, with N the release version number.
  https://github.com/juhp/fedora-repoquery#readme

Available options:
  -h,--help                Show this help text
  --version                Show version
  -q,--quiet               Avoid output to stderr
  -v,--verbose             Show stderr from dnf repoquery
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
fedora-repoquery can be installed from
[copr](https://copr.fedorainfracloud.org/coprs/petersen/fedora-repoquery/)

## Building from source
Use `stack install fedora-repoquery` or `cabal install fedora-repoquery`
to build the latest release.

To build from git: `stack install` or `cabal install`.

## Contributing
fedora-repoquery is distributed under the GPL license version 3 or later.

<https://github.com/juhp/fedora-repoquery>
