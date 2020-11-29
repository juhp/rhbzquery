# rhbzquery

A cli tool for querying bugzilla.redhat.com.

The tool outputs the bugzilla query url,
and if xdg-open is available will try to open the url
(unless --dryrun is given).

## Usage examples

Your open bugs: `rhbzquery --mine`

F33 bugs for package xyz: `rhbzquery f33 xyz`

Closed rawhide bugs for package xyz: `rhbzquery closed rawhide xyz`

RHEL8 bash bugs: `rhbzquery rhel8 bash`

All your open and closed approved reviews: `rhbzquery --mine all flag=fedora-review+`
