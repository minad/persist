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
| **cereal**    |     **1.2** |
| binary    |     7.2 |
| serialise |     8.4 |

#### deserialization (time)/BinTree Int (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.0** |
| **cereal**    |     **1.2** |
| binary    |     4.3 |
| serialise |     5.0 |

#### deserialization (time)/Cars (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.3** |
| cereal    |     1.6 |
| binary    |     8.0 |
| serialise |     9.9 |

#### deserialization (time)/Iris (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.2** |
| cereal    |     3.0 |
| serialise |     3.5 |
| binary    |     7.6 |

#### deserialization (time)/[Direction] (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| cereal    |     1.3 |
| store     |     1.5 |
| binary    |     4.4 |
| serialise |     6.9 |

#### serialization (time)/BinTree Direction (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.1** |
| cereal    |     8.2 |
| binary    |    16.1 |
| serialise |    26.4 |

#### serialization (time)/BinTree Int (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| **persist**   |     **1.0** |
| cereal    |    10.7 |
| binary    |    18.3 |
| serialise |    25.3 |

#### serialization (time)/Cars (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| persist   |     2.1 |
| cereal    |     3.1 |
| binary    |     9.0 |
| serialise |    27.1 |

#### serialization (time)/Iris (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| persist   |     3.8 |
| cereal    |     4.8 |
| serialise |    21.4 |
| binary    |    37.2 |

#### serialization (time)/[Direction] (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| persist   |     2.0 |
| cereal    |     2.6 |
| binary    |     2.9 |
| serialise |     7.7 |
