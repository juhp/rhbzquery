# rhbzquery

A cli tool for querying bugzilla.redhat.com.

The tool outputs the bugzilla query url,
and if xdg-open is available will try to open the url
(unless --dryrun is given).

## Usage examples

`rhbzquery f33 xyz` : F33 bugs for package xyz

`rhbzquery closed rawhide xyz` : closed rawhide bugs for package xyz

`rhbzquery --mine` : your open bugs (gets userid from `.bugzillarc`)

`rhbzquery rhel8.3 bash` : RHEL 8.3 bash bugs

`rhbzquery "Package Review" reporter_realname="Your Name"` : open package reviews you reported

`rhbzquery --mine all flag~fedora-review+` : all open and closed approved reviews you reviewed

`rhbzquery component~bugzilla summary~bugzilla` : open bugs with component and summary containing "bugzilla"

`rhbzquery --file f33 xyz` : file a bug against the xyz package in F33

### Help
`rhbzquery --help` describes arguments

`rhbzquery --list-fields` : lists the many fields (not all well supported yet, eg timestamps).

## Installation
Run `stack install` or `cabal install`.

Binary packages for Fedora are available from <https://copr.fedorainfracloud.org/coprs/petersen/rhbzquery/>.

## Requests and feedback
Feature requests, bug reports and contributions are welcome.

Please open an issue at <https://github.com/juhp/rhbzquery>.
