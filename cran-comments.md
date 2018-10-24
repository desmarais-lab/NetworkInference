Version 1.2.3

Fixed potential memory error from iterating over the beginning/end of a `std::map`

## Test environments
* Mac OS 10.12.6 (High Sierra), R 3.5.1
* Ubuntu 16.04.5 LTS, R 3.5.1
* Ubuntu 14.04.5 LTS (travis-ci), R 3.5.1 (+ valgrind)
* win-builder (devel and release)

## R CMD check results

* No errors, warnings

* I got one note from R-hub about a possibly invalid URL: 'http://snap.stanford.edu/netinf/'. It seems like the site is temporarilly down but the URL is valid.

## Downstream dependencies
There are no downstream dependencies