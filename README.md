# Minimal binary serialization library with focus on performance

[![Hackage](https://img.shields.io/hackage/v/persist.svg)](https://hackage.haskell.org/package/persist)
[![Build Status](https://secure.travis-ci.org/minad/persist.png?branch=master)](http://travis-ci.org/minad/persist)

persist is a reasonably fast binary serialization library operating on strict ByteStrings with small dependency footprint.
The default binary format uses the little endian representation on both big and little endian machines.
The API design is mostly compatible with the cereal library. However the binary format
is not compatible with binary and cereal. Its internal machinery for deserialization is based on the `store` library.
Serialization generates ByteStrings directly instead of relying on the ByteString Builder.

The interface is rich enough to allow for a `Perist` instance to match an
externally specified serialization format. 1.0.0.0 Has helper functions that
make this easier.

## Comparison with other libraries

* store - faster serialization, machine dependent, larger library, many dependencies. Difficult to use for external formats.
* cereal - similar to persist, slower
* binary - lazy and slower than persist

`persist` consistently has faster deserialization performance. There is likely
more that can be done to improve the `Generic` driven default serialization.

## Benchmarks

Benchmarks are available at https://github.com/haskell-perf/serialization.
The following serialization and deserialization results were measured on GHC 9.12.2
Results that are within 30% of the best result are displayed in **bold**.

#### deserialization (time)/BinTree Direction (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.1** |
| cereal    |     1.4 |
| binary    |    10.5 |
| serialise |    20.1 |

#### deserialization (time)/BinTree Int (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.1** |
| cereal    |     1.4 |
| binary    |     5.3 |
| serialise |    12.1 |

#### deserialization (time)/Cars (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.1** |
| cereal    |     1.6 |
| binary    |    12.2 |
| serialise |    26.3 |

#### deserialization (time)/Iris (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.1** |
| cereal    |     2.7 |
| serialise |     7.7 |
| binary    |    10.4 |

#### deserialization (time)/[Direction] (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.1** |
| **cereal**    |     **1.1** |
| binary    |     5.5 |
| serialise |    10.8 |

#### serialization (time)/BinTree Direction (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.1** |
| cereal    |     7.9 |
| binary    |    25.0 |
| serialise |    45.1 |

#### serialization (time)/BinTree Int (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.3** |
| cereal    |     9.8 |
| binary    |    26.3 |
| serialise |    35.5 |

#### serialization (time)/Cars (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| persist   |     1.4 |
| cereal    |     3.0 |
| binary    |    16.4 |
| serialise |    44.4 |

#### serialization (time)/Iris (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| persist   |     1.4 |
| cereal    |     2.8 |
| serialise |    25.7 |
| binary    |    29.8 |

#### serialization (time)/[Direction] (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.2** |
| cereal    |     3.7 |
| binary    |     4.7 |
| serialise |    14.4 |
