# Revision history for fedora-repoquery

## 0.4 (2024-05-24)
- add --dnf4 option to use dnf4 (actually /usr/bin/dnf-3) instead of dnf5
- rawhide and oldest active releases are now determined/cached via Bodhi API
- can now query with rawhide version (eg 41 currently)
- --requires do not allow --qf
- --list now shows active fedora releases
- for centos and eln only check compose via BaseOS repo
- map version 11 to eln

## 0.3.2 (2024-02-19)
- enable centos 10 stream (still alpha)
- Query: disable --qf for --provides
- minor debug tweaks

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
