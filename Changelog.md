## Changes from 0.1.1.5 to 1.0.0.0 ##

  * Use DerivingVia to reduce repetititon for base instances
    
    Add three helper type classes: HasEndianness, SerializeAs,
    ReinterpretAs, and two newtypes: ViaSerializeAs and ViaReinterpretAs.
    HasEndianness allows us to create persist instances for `BigEndian a`
    and `LittleEndian a` without repetititon. SerializeAs allows anyone to
    supply a cast function in both directions and then re-use either a
    HasEndianness instance or a Persist instance. See the source file for
    example uses.

  * Add unsafe versions of put/get and helpers
    
    Allow for Persist instances to reserve specific sizes ahead of time and
    then elide the checks while serializing. As an implemented example, we
    allocate space for the length of a ByteString and the bytes in the
    ByteString together. For characters as well, we allocate the space all
    at once. This technique would be useful for arrays of fixed size
    elements, like unboxed vectors.

  * Add getPrefix and unsafeGetPrefix
    
    It often occurs when deserializing a custom format that we need to
    recursively deserialize a subcomponent from the next `n` bytes. The
    naive way to do this would be to get a `ByteString` for those n bytes,
    and then run the appropriate `Get` on that `ByteString`. To simplify
    this process, introduce `getPrefix` and `unsafeGetPrefix`. These
    manipulate the `Get` internals to expose only the appropriate bytes to
    the sub-deserializer. This avoids repeated copying, and even repeated
    `ByteString` construction.

  * Add length backpatching
    
    There are many serialized formats where a serialized sub-value is
    prefixed by its length. The simplest way to find the serialized length
    of a value is to serialize it and then to measure the length. For
    formats that use a fixed number of bytes for this length, we can avoid
    copying and also walking a data structure twice: Once to find the size,
    and again to write the data with the known size. It is straightforward
    to use this. Reserve the size, serialize the sub-value, then resolve the
    size, with the correct endianness and inclusivity.
    
    We do this by reserving the bytes ahead of time and returning a
    `PutSize` handle that can be used later to fill in the result.
    Currently, the library handles computing the appropriate size, which can
    be filled in either Big or Little endian, and either inclusive or
    exclusive of the size itself. DHCPv6 packets use this style of
    length-value encoding. As does Cassandra's encoding format, and MongoDB
    as well. MongoDB uses this same backpatching optimization.

  * Switch to NoFieldSelectors and OverloadedRecordDot
    
    GHC has supported OverloadedRecordDot and NoFieldSelectors since 9.2
    Switch away from prefixed names and instead just use names and record
    dot syntax. There are a handful of now-ambiguous updates. They were
    resolved using a small lambda and RecordWildCards/NamedFieldPuns. This
    is a breaking change, but there are several and we are planning on a new
    major release.

  * Bump version bounds to match ghc 9.2
    
    Bump tested-with to match the versions of ghc 9.2--9.12 that were
    manually tested. Bump versions of any libraries included with ghc to
    match the minimum version included with ghc 9.2

  * Add Kyle Butt as maintainer and author

  * Borrow improved error message handling from Store

  * Improve interaction with bytestring library
    
    Avoid a copy for a strict ByteString with only one chunk by calling
    `reallocBytes` and then building the ByteString directly by recording a
    finalizer.
    
    Add support for running a put and producing a lazy ByteString. Turn each
    chunk into a strict ByteString with the appropriate finalizer, and then
    use `foldlM` to reverse the chunks and build the lazy ByteString at the
    same time.

  * Add resolveSize helpers that write a computed size
    
    This makes list serialization much faster. It makes almost all of the
    serialization benchmarks competitive with store. The remaining gap is
    due to reinterpretCast. Copying a Double to tmp and then again to the
    buffer is slower. But this is a simple win that puts the new reservation
    code to good use.
