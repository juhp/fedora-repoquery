# fedora-repoquery

A work-in-progress wrapper for dnf repoquery.

## Usage
Usage examples:

`fdrq rawhide firefox`

`fdrq f38 --requires podman`

etc

```shellsession
$ fdrq --version
0.1.0
$ fdrq --help
Fedora repoquery tool

Usage: fdrq [--version] [(-q|--quiet) | (-v|--verbose)] [-K|--koji]
            [--centos-devel | --test] [(-m|--mirror URL) | (-D|--dl)]
            [(-s|--source) | (-a|--arch ARCH)] [-d|--debug]
            ((-z|--cache-size) | (-e|--cache-clean-empty) | (-l|--list) |
              RELEASE [[REPOQUERY_OPTS] [PACKAGE]...])
  Tool for querying Fedora repos for packages.

Available options:
  -h,--help                Show this help text
  --version                Show version
  -q,--quiet               Avoid output to stderr
  -v,--verbose             Show stderr from dnf repoquery
  -K,--koji                Use Koji buildroot
  --centos-devel           Use centos-stream development compose
  --test                   Use centos-stream test compose [default: production]
  -m,--mirror URL          Fedora mirror [default:
                           https://download.fedoraproject.org/pub]
  -D,--dl                  Use dl.fp.o
  -s,--source              Query source repos
  -a,--arch ARCH           Specify arch [default: x86_64]
  -d,--debug               Show some debug output
  -z,--cache-size          Show total dnf repo metadata cache disksize
  -e,--cache-clean-empty   Remove empty dnf caches
  -l,--list                List Fedora versions
```

## Installation
fedora-repoquery can be installed from
[copr](https://copr.fedorainfracloud.org/coprs/petersen/fedora-repoquery/)

## Building from source
Use `stack install` or `cabal install`.

## Contributing

fedora-repoquery is distributed under the GPL license version 3 or later.
