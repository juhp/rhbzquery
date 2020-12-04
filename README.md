# rhbzquery

A cli tool for querying bugzilla.redhat.com.

The tool outputs the bugzilla query url,
and if xdg-open is available will try to open the url
(unless --dryrun is given).

## Usage examples

`rhbzquery --mine`: your open bugs (reads `.bugzillarc`)

`rhbzquery f33 xyz`: F33 bugs for package xyz

`rhbzquery closed rawhide xyz`: closed rawhide bugs for package xyz

`rhbzquery rhel8.3 bash`: RHEL 8.3 bash bugs

`rhbzquery "Package Review" reporter_realname="Your Name"`: open package reviews you reported

`rhbzquery --mine all flag=fedora-review+`: all your open and closed approved reviews

`rhbzquery summary=bugzilla`: open bugs with summary containing "bugzilla":

### Help
`rhbzquery --help` lists the many fields (note: not all well supported yet, eg timestamps).

## Installation
Run `stack install` or `cabal install`.

## Requests and feedback
Feature requests, bug reports and contributions are welcome.

Please open an issue at <https://github.com/juhp/rhbzquery>.
