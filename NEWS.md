# schwabr 0.1.2

## Release Notes and News

### schwabr 0.1.2 - 2/10/2025

Updating based on feedback from CRAN to quote APIs, software, and packages. 
Schwab is not quoted because it references a company/account, but 'Schwab API'
is quoted along with references to 'schwabr'. Added a return in pipe, and 
removing print statements. It was requested to remove \dontrun{} but this is
not possible because then all examples would fail. 

### schwabr 0.1.1 - 2/7/2025 888cffa9350

Removing License file and modifying description to match

### schwabr 0.1.0 - 2/7/2025 31e05238

Initial release includes basic functionality and API calls. Authentication,
Placing Orders, Canceling Orders, and pulling account details are all included
with the initial release.

Updating User Preferences and complex order entry are not included. Complex
option chains are also not included. 

Disclosure: 
This software is in no way affiliated, endorsed, or approved by Charles Schwab
or any of its affiliates. It comes with absolutely no warranty and
should not be used in actual trading unless the user can read and understand the
source code. The functions within this package have been tested under basic
scenarios. There may be bugs or issues that could prevent a user from executing
trades or canceling trades. It is also possible trades could be submitted in
error. The user will use this package at their own risk.
