## Test environments
* local Mac OS X, R 4.4.2
* ubuntu 22.04, R 4.4.2
* Windows using check_win_devel

## R CMD check results
There were no ERRORs or WARNINGs. 

## Downstream dependencies
All tests are specifically blocked 
from CRAN because they require account authentication 
to be run successfully. 
Examples are all blocked except for the URL generation
for the same reason. The examples will all fail if run 
without authenticating to the API first.

## Other Comments

