# * Bits

module Bits

export bitsize, bits

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
       bitsize(big(0)) == Bits.INF     &&
       bitsize(1.2)    == 64
true
```
"""
bitsize(T::BitIntegerType) = sizeof(T) * 8
bitsize(::Type{BigInt}) = INF
bitsize(::Type{Bool}) = 1
bitsize(T::Union{Type{Float16},Type{Float32},Type{Float64}}) = sizeof(T) * 8
bitsize(T::Type) = throw(MethodError(bitsize, (T,)))
bitsize(x) = bitsize(typeof(x))

lastactualpos(x::Integer) = bitsize(x)
lastactualpos(x::BigInt) = abs(x.size) * sizeof(Base.GMP.Limb) * 8


# * bits & BitVector1

"""
    bits(x::Integer)

Create an immutable view on the bits of `x` as a vector of `Bool`, similar to a `BitVector`.
If `x` is a `BigInt`, the vector has length [`Bits.INF`](@ref).
Currently, no bounds check is performed when indexing into the vector.

# Examples
```jldoctest
julia> v = bits(Int16(2^8+2^4+2+1))
|00000001 00010011|

julia> permutedims([v[i] for i in 8:-1:1])
1×8 Array{Bool,2}:
 false  false  false  true  false  false  true  true

julia> bits(true)
|1|

julia> bits(big(2)^63)
|...0 10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000|
```
"""
bits(x::Integer) = BitVector1(x)


# ** BitVector1

# similar to a BitVector, but with only 1 word to store bits (instead of 1 array thereof)
struct BitVector1{T<:Integer} <: AbstractVector{Bool}
    x::T
end

Base.size(v::BitVector1) = (bitsize(v.x),)
Base.getindex(v::BitVector1, i::Integer) = (v.x >> (UInt(i)-1)) % Bool


# ** show

function Base.show(io::IO, v::BitVector1)
    if v.x isa Bool
        return print(io, "|$(Int(v[1]))|")
    elseif v.x isa BigInt
        print(io, "|...", v.x < 0 ? "1 " : "0 ")
    else
        print(io, "|")
    end
    for i = lastactualpos(v.x):-1:1
        show(io, v[i] % Int)
        i % 8 == 1 && i != 1 && print(io, ' ')
    end
    print(io, "|")
end

Base.show(io::IO, ::MIME"text/plain", v::BitVector1) = show(io, v)


end # module
