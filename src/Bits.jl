module Bits

export bitsize

using Base: BitIntegerType


# * constants

const Index = Int # do not change

"""
`INF::Int` indicates the position of the bit at "infinity", for types
which can carry an arbitrary number of bits, like BigInt.
`INF` is also used to indicate an arbitrary large number of bits.
Currently, `Bits.INF == typemax(Int)`.
"""
const INF = typemax(Index)


# * bitsize

"""
    bitsize(T::Type) -> Int
    bitsize(::T)     -> Int

Return the number of bits that can be held by type `T`.

# Examples
```jldoctest
julia> bitsize(Int32)  == 32           &&
       bitsize(true)   == 1            &&
       bitsize(big(0)) == Bits.INF
true
```
"""
bitsize(T::BitIntegerType) = sizeof(T) * 8
bitsize(::Type{BigInt}) = INF
bitsize(::Type{Bool}) = 1
bitsize(T::Type) = throw(MethodError(bitsize, (T,)))
bitsize(x) = bitsize(typeof(x))

end # module
