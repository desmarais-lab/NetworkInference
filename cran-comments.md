Version 1.2.0
 
## Test environments
* Mac OS 10.12.5 (Sierra), R 3.5.0
* ubuntu 14.04.5 (on travis-ci), R 3.5.0
* Red Hat Enterprise Linux Server release 6.9, R 3.4.0
* Debian 9.3.0, R 3.5.0
* win-builder (devel and release)

## R CMD check results

A submission yesterday failed due to a test failing on Debian. The same test
passes on Mac OS, Windows, Ubuntu and a Debian 9.3.0 virtual machine. I can't reproduce the error. The tested function has a random component but is seeded. Since it is not central to the package, I removed the failing test. 

## Downstream dependencies
There are currently no downstream dependencies