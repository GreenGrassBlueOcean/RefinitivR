## Test environments

* local Windows 11 x64, R 4.6.0
* GitHub Actions (ubuntu-latest, macos-latest, windows-latest; R-release, R-devel, R-oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission (0.2.4).
* API examples that require a running 'LSEG Workspace' / 'Refinitiv Eikon' desktop terminal and active commercial API credentials are wrapped in `\dontrun{}`. Offline helper functions (`rd_CacheInfo()`, `rd_ClearCache()`, `EikonNameCleaner()`, `EikonRepairMic()`, etc.) have runnable examples, and the test suite verifies all endpoints offline using recorded `httptest2` fixtures.
