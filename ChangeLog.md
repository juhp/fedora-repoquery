# Revision history for fedora-repoquery

## 0.3.1 (2024-02-12)
- use dnf5 when available
- add --all-archs to query across all 64bit architectures

## 0.3 (2023-11-09)
- Query: no --qf with --changelog
- small OS versions now map to centos-stream
- use hostname not full url for mirror repo name
- repo urls should end with a slash
- improve tests to fail if no results
- map failing yzyu.jp http/3 mirror to main one
- development/39 also needs os/
- showRelease: reformat Centos and ELN repos
- default to system arch and introduce sysarch and march

## 0.2 (2023-05-12)
- support fedora and epel testing repos
- fix fedora archive urls
- support querying epel6 and older archives
- support fedora-secondary urls for ppc64le and s390x including archive
- rename --test to --test-channel and --centos-devel to --devel-channel
- expand help to cover RELEASE and add readme url

## 0.1.0 (2023-05-07)
- initial release with support for fedora releases, epel, eln and centos stream
