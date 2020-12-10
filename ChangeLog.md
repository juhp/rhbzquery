# rhbzquery releases

## 0.3 (2020-12-10)
- add --list-fields option
- validate name of fields
- for 'content' field map '~'/'!~' to matches/notmatches
- refactor operator handling to generate help

## 0.2 (2020-12-09)
- support most search type operators (regexp, substr, isempty, and negatives)

## 0.1.1 (2020-12-04)
- --file a bug
- --help: do not format FIELDS
- add testsuite and travis

## 0.1.0 (2020-12-02)
- initial release: supports field parameters, status, flags
- has --mine
- opens urls with xdg-open
