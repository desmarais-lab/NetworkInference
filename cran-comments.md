Version 1.2.4

Fixed potential memory error from iterating over the beginning/end of a `std::map`. The last fix was apparently not enough. I was able to reproduce the issue using an ASAN instrumented R version and could resolve it. Hopefully the end of this issue.

## Test environments
* Mac OS 10.12.6 (Mojave), R-devel instrumented with ASAN following instructions at https://www.stats.ox.ac.uk/pub/bdr/memtests/README.txt
* Mac OS 10.12.6 (Mojave), R 3.5.2
* Ubuntu 16.04.5 LTS (with valgrind), R 3.5.2
* Ubuntu 14.04.5 LTS (travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

* No errors, warnings, notes

## Downstream dependencies
There are no downstream dependencies