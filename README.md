[![Travis-CI Build Status](https://travis-ci.org/flinder/NetworkInference.svg?branch=master)](https://travis-ci.org/flinder/NetworkInference)

# NetworkInference

## About

R interface to the stanford network analysis project's ([SNAP](https://github.com/snap-stanford/snap)) [netinf](https://github.com/snap-stanford/snap/tree/master/examples/netinf) algorithm.

## Use
```R
devtools::install_github('flinder/NetworkInference')
library(NetworkInference)

data(cascades)
out <- netinf(cascades, trans_mod = "exponential", alpha = 1, verbose = TRUE)
```

## Notes on the snap source

The `snap` source is mostly untouched. Due to compilation problems in Windows 
which will hopefully be fixed soon, the source has been changed in the following 
places:

* In `glib-core/bd.cpp`, commented out line 5-37 due to a declaration conflict for `_matherr`
* In `glib-core/dt.cpp`, replaced in lines 2143-2144:

```c++
const TUInt64 TUInt64::Mn(uint64(0x0000000000000000i64));
const TUInt64 TUInt64::Mx(uint64(0xFFFFFFFFFFFFFFFFi64));
```
with
```c++
const TUInt64 TUInt64::Mn((uint64)0x0000000000000000LL);
const TUInt64 TUInt64::Mx(0xFFFFFFFFFFFFFFFFLL);
```