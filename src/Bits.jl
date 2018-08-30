# * Bits

module Bits

export bitsize, bits, bit, tstbit, mask

using Base: BitInteger, BitIntegerType


# * constants

const Index = Int # do not change

const Word = UInt # default integer type

"""
`INF::Int` indicates the position of the bit at "infinity", for types
which can carry an arbitrary number of bits, like BigInt.
`INF` is also used to indicate an arbitrary large number of bits.
Currently, `Bits.INF == typemax(Int)`.
"""
const INF = typemax(Index)

const BitFloats = Union{Float16,Float32,Float64}


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

lastactualpos(x::Union{Integer,BitFloats}) = bitsize(intfallback(x))
lastactualpos(x::BigInt) = abs(x.size) * sizeof(Base.GMP.Limb) * 8

intfallback(x::Integer) = x
intfallback(x::BitFloats) = reinterpret(Signed, x)


# * bit functions: bit, tstbit, mask

# ** bit

"""
    bit(x::Integer, i::Integer)       -> typeof(x)
    bit(x::AbstractFloat, i::Integer) -> Integer

Return the bit of `x` at position `i`, with value `0` or `1`.
If `x::Integer`, the returned bit is of the same type.
If `x::AbstractFloat` is a bits type, the returned bit is a signed integer with the same [`bitsize`](@ref) as `x`.
See also [`tstbit`](@ref).

# Examples
```jldoctest
julia> bit(0b101, 1)
0x01

julia> bit(0b101, 2)
0x00

julia> bit(-1.0, 64)
1
```
"""
bit(x::Integer, i::Integer) = (x >>> UInt(i-1)) & one(x)
bit(x::BitFloats, i::Integer) = bit(intfallback(x), i)
bit(x::BigInt, i::Integer) = tstbit(x, i) ? big(1) : big(0)


# ** tstbit

"""
    tstbit(x::Real, i::Integer) -> Bool

Similar to [`bit`](@ref) but returns the bit at position `i` as a `Bool`.

# Examples
```jldoctest
julia> tstbit(0b101, 3)
true
```
"""
tstbit(x, i::Integer) = bit(x, i) % Bool
tstbit(x::BigInt, i::Integer) = Base.GMP.MPZ.tstbit(x, i-1)


# ** mask

"""
    mask(T::Type{<:Integer}:=UInt, i::Integer=bitsize(T)) -> T

Return an integer of type `T` whose `i` right-most bits are `1`, and the
others are `0` (i.e. of the form `0b0...01...1` with exactly `i` `1`s.
When `i` is not specified, all possible bits are set to `1`.
When `i < 0`, the result is not specified.
`T` defaults to `UInt`.

# Examples
```jldoctest
julia> mask(3)
0x0000000000000007

julia> mask(UInt8)
0xff

julia> bits(mask(Int32, 24))
|00000000 11111111 11111111 11111111|
```
"""
mask(::Type{T}, i::Integer) where {T} = one(T) << i - one(T)

# alternate implementation:
mask_2(T::BitIntegerType, i::Integer) = let s = bitsize(T)-i
    mask(T) << s >>> s
end

mask(i::Integer) = mask(Word, i)

mask(::Type{T}=Word) where {T} = ~zero(T)

# TODO: optimize
mask(::Type{BigInt}, i::Integer) = one(BigInt) << i - 1

"""
    mask(T::Type{<:Integer} := UInt, j::Integer, i::Integer) -> T

Return an integer of type `T` whose `j` right-most bits are `0`, the
following `i-j` bits are `1`, and the remaining bits are `0`
(i.e. of the form `0b0...01...10...0` with exactly `i-j` `1`s preceded by
`j` `0`s).
When `j < 0`, the result is not specified.
When `i < 0`, the result is equal to `~mask(T, j)`, i.e. of the form
`1...10...0` with exactly `j` zeros.
NOTE: unstable API, could be changed to mask(j, i-j) instead.

# Examples
```jldoctest
julia> bits(mask(UInt8, 2, 5))
|00011100|

julia> bits(mask(BigInt, 3, -1))
|...1 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111000|
```
"""
mask(::Type{T}, j::Integer, i::Integer) where {T} = mask(T, i-j) << j

# alternate implementation
mask_2(::Type{T}, j::Integer, i::Integer) where {T} = mask(T, i) & ~mask(T, j)

mask(j::Integer, i::Integer) = mask(Word, j, i)


# * bits & BitVector1

"""
    bits(x::Real)

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

julia> bits(Float32(-7))
|1 10000001 11000000000000000000000|

julia> ans[1:23] # creates a vector of bits with a specific length
|1100000 00000000 00000000|
```
"""
bits(x::Real) = BitVector1(x)


# ** BitVector1

# similar to a BitVector, but with only 1 word to store bits (instead of 1 array thereof)
abstract type AbstractBitVector1 <: AbstractVector{Bool} end

struct BitVector1{T<:Real} <: AbstractBitVector1
    x::T
end

struct BitVector1Mask{T<:Real} <: AbstractBitVector1
    x::T
    len::Int
end

Base.size(v::BitVector1) = (bitsize(v.x),)
Base.size(v::BitVector1Mask) = (v.len,)
Base.getindex(v::AbstractBitVector1, i::Integer) = tstbit(v.x, i)

function Base.getindex(v::AbstractBitVector1, a::AbstractVector{<:Integer})
    xx, _ = foldl(a, init=(zero(intfallback(v.x)), 0)) do xs, i
        x, s = xs
        (x | bit(v.x, i) << s, s+1)
    end
    BitVector1Mask(xx, length(a))
end


# ** show

spaceafter(x, i) = i % 8 == 1
spaceafter(x::Float16, i) = i in (11, 16)
spaceafter(x::Float32, i) = i in (24, 32)
spaceafter(x::Float64, i) = i in (53, 64)

function Base.show(io::IO, v::AbstractBitVector1)
    if v.x isa BigInt && v isa BitVector1
        print(io, "|...", v.x < 0 ? "1 " : "0 ")
    else
        print(io, "|")
    end
    l = v isa BitVector1 ? lastactualpos(v.x) : v.len
    for i = l:-1:1
        show(io, v[i] % Int)
        spaceafter(v.x, i) && i != 1 && print(io, ' ')
    end
    print(io, "|")
end

Base.show(io::IO, ::MIME"text/plain", v::AbstractBitVector1) = show(io, v)


end # module
