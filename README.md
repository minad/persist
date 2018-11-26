# Minimal binary serialization library with focus on performance

[![Hackage](https://img.shields.io/hackage/v/persist.svg)](https://hackage.haskell.org/package/persist)
[![Build Status](https://secure.travis-ci.org/minad/persist.png?branch=master)](http://travis-ci.org/minad/persist)

persist is a reasonably fast binary serialization library operating on strict ByteStrings with small dependency footprint.
The binary format uses the little endian representation on both big- and little endian machines.
The API design is mostly compatible with the cereal library. However the binary format
is not compatible with binary and cereal. Its internal machinery for deserialization is based on the `store` library.
Serialization generates ByteStrings directly instead of relying on the ByteString Builder.

## Comparison with other libraries

* flat - bit packing (!), fast, longer compile times
* store - faster serialization, larger library, many dependencies
* cereal - similar to persist, slower
* binary - lazy and slower than persist

## Benchmarks

Benchmarks are available at https://github.com/haskell-perf/serialization.
The following serialization and deserialization results were measured on GHC 8.6.2.
Results that are within 30% of the best result are displayed in **bold**.

#### deserialization (time)/BinTree Direction (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.1** |
| **flat**      |     **1.2** |
| cereal    |     4.3 |
| serialise |     6.1 |
| binary    |     7.8 |
| packman   |    13.5 |

#### deserialization (time)/BinTree Int (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| **persist**   |     **1.1** |
| **cereal**    |     **1.3** |
| flat      |     1.3 |
| serialise |     4.1 |
| binary    |     4.8 |
| packman   |    15.0 |

#### deserialization (time)/Cars (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **store**     |     **1.1** |
| flat      |     1.3 |
| cereal    |     2.9 |
| packman   |     3.7 |
| serialise |     4.8 |
| binary    |     7.2 |

#### deserialization (time)/Iris (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| **persist**   |     **1.1** |
| **flat**      |     **1.2** |
| serialise |     2.4 |
| cereal    |     3.6 |
| packman   |     3.6 |
| binary    |     8.5 |

#### deserialization (time)/[Direction] (best first)

| package | performance |
| ---| ---|
| **persist**   |     **1.0** |
| **flat**      |     **1.1** |
| **store**     |     **1.2** |
| **cereal**    |     **1.3** |
| serialise |     3.0 |
| binary    |     3.1 |
| packman   |    11.0 |

#### serialization (time)/BinTree Direction (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| **persist**   |     **1.1** |
| flat      |     1.4 |
| cereal    |     8.4 |
| binary    |    16.0 |
| serialise |    22.0 |
| packman   |    38.4 |

#### serialization (time)/BinTree Int (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| **flat**      |     **1.0** |
| persist   |     1.7 |
| cereal    |    14.1 |
| binary    |    18.7 |
| serialise |    20.6 |
| packman   |    54.4 |

#### serialization (time)/Cars (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| flat      |     2.1 |
| persist   |     3.6 |
| cereal    |     5.7 |
| binary    |    11.2 |
| serialise |    13.7 |
| packman   |    15.6 |

#### serialization (time)/Iris (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| flat      |     5.0 |
| persist   |     6.4 |
| serialise |    12.5 |
| cereal    |    15.0 |
| packman   |    36.0 |
| binary    |    80.0 |

#### serialization (time)/[Direction] (best first)

| package | performance |
| ---| ---|
| **store**     |     **1.0** |
| **persist**   |     **1.1** |
| flat      |     1.4 |
| cereal    |     3.8 |
| binary    |     5.2 |
| serialise |     7.8 |
| packman   |    45.0 |
