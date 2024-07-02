# fedora-repoquery

A fedora release version wrapper of dnf repoquery,
which caches repodata separately per release.

## Usage
Usage examples:

`$ fdrq rawhide firefox`

```
firefox-127.0.2-1.fc41.x86_64 (fedora-rawhide)
```

`$ fdrq 40 --requires filesystem`

```
setup
```

`$ fdrq epel9 ghc`

```
ghc-8.10.7-116.el9.x86_64 (epel9)
```

`$ fdrq c10 bash`

```
bash-5.2.26-3.el10.x86_64 (c10s-BaseOS)
```

`$ fdrq eln kernel`

```
kernel-6.10.0-0.rc6.50.eln141.x86_64 (eln-BaseOS)
```

etc.

The above output is generated with mdsh which suppresses the stderr
that includes mirror repo urls with compose timestamps, like this:
```
$ fdrq rawhide fedrq
2024-05-23 16:41:58 +08 <https://mirror.freedif.org/fedora/fedora/linux/development/rawhide>

fedrq-1.1.0-1.fc41.noarch (fedora-rawhide)
```

Also note that dnf5 currently still outputs repo update messages to stdout
but it is a lot faster than dnf4.

## Help
`$ fdrq --version`

```
0.5
```
`$ fdrq --help`

```
fedora-repoquery tool for querying Fedora repos for packages.

Usage: fdrq [--version] [-4|--dnf4] [(-q|--quiet) | (-v|--verbose)] [--quick] 
            [-K|--koji] [--devel-channel | --test-channel] 
            [(-m|--mirror URL) | (-D|--dl)] 
            [(-s|--source) | (-A|--all-archs) | [-a|--arch ARCH]] [-t|--testing]
            [-d|--debug] 
            ((-z|--cache-size) | (-e|--cache-clean-empty) | (-l|--list) | 
              RELEASE... [REPOQUERY_OPTS]... [PACKAGE]...)

  where RELEASE is {fN or N (fedora), 'rawhide', epelN, epelN-next, cN (centos
  stream), 'eln'}, with N the release version number.
  https://github.com/juhp/fedora-repoquery#readme

Available options:
  -h,--help                Show this help text
  --version                Show version
  -4,--dnf4                Use dnf4 instead of dnf5 (if available)
  -q,--quiet               Avoid output to stderr
  -v,--verbose             Show stderr from dnf repoquery
  --quick                  Skip http repo url checks
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

To build from git: `stack install` or `cabal install` or `cabal-rpm install`.

## Contributing
fedora-repoquery is distributed under the GPL license version 3 or later.

<https://github.com/juhp/fedora-repoquery>
