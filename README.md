# rhbzquery

A cli tool for querying bugzilla.redhat.com.

The tool outputs the bugzilla query url,
and if xdg-open is available will try to open the url
(unless --dryrun is given).

## Usage examples

Your open bugs: `rhbzquery --mine`
(reads `.bugzillarc`)

F33 bugs for package xyz: `rhbzquery f33 xyz`

Closed rawhide bugs for package xyz: `rhbzquery closed rawhide xyz`

RHEL 8.3 bash bugs: `rhbzquery rhel8.3 bash`

Open Package Reviews you reported: `rhbzquery "Package Review" reporter_realname="Your Name"`

All your open and closed approved reviews: `rhbzquery --mine all flag=fedora-review+`

Open bugs with summary containing "bugzilla": `rhbzquery summary=bugzilla`

## Help
`rhbzquery --help` lists the many fields (note: not all well supported yet, eg timestamps).

## Requests and feedback
Feature requests, bug reports and contributions are welcome.

Please open an issue at <https://github.com/juhp/rhbzquery>.
