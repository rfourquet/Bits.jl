using Bits , Test

x ≜ y = typeof(x) == typeof(y) && x == y

@testset "bitsize" begin
    for T in Base.BitInteger_types
        @test bitsize(T) === sizeof(T) * 8
        @test bitsize(zero(T)) === bitsize(one(T)) === bitsize(T)
    end
    @test bitsize(BigInt) === Bits.INF
    @test bitsize(Bool) === 1
    @test bitsize(Float64) === 64
    @test bitsize(Float32) === 32
    @test bitsize(Float16) === 16
    @test_throws MethodError bitsize(BigFloat)
end

@testset "bit functions" begin
    @testset "bit & tstbit" begin
        val(::typeof(bit), x) = x
        val(::typeof(tstbit), x) = x % Bool
        for _bit = (bit, tstbit)
            for T = (Base.BitInteger_types..., BigInt)
                T0, T1 = T(0), T(1)
                @test _bit(T(0), rand(1:bitsize(T))) ≜ val(_bit, T0)
                @test _bit(T(1), 1) ≜ val(_bit, T1)
                @test _bit(T(1), 2) ≜ val(_bit, T0)
                @test _bit(T(5), 1) ≜ val(_bit, T1)
                @test _bit(T(5), 2) ≜ val(_bit, T0)
                @test _bit(T(5), 3) ≜ val(_bit, T1)
                @test _bit(T(5), 4) ≜ val(_bit, T0)
            end
            @test _bit( 1.0, 64) ≜ val(_bit, 0)
            @test _bit(-1.0, 64) ≜ val(_bit, 1)
            @test _bit( Float32(1.0), 32) ≜ val(_bit, Int32(0))
            @test _bit(-Float32(1.0), 32) ≜ val(_bit, Int32(1))
        end
    end
end

@testset "bits" begin
    for T in Base.BitInteger_types
        v = bits(one(T))
        @test length(v) == bitsize(T)
        @test v[1] === true
        @test count(v) == 1
        v = bits(zero(T))
        @test length(v) == bitsize(T)
        @test v[1] === false
        @test count(v) == 0
    end
    v = bits(7)
    @test v[1] === v[2] === v[3] === true
    @test count(v) == 3
    for T = (Int64, UInt64)
        v = bits(T(2)^63)
        @test v[64] === true
        @test count(v) == 1
    end
    @test bits(Float64(-16))[53:end] == [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1]
    @test count(bits(Float64(-16))) == 4

    @testset "array indexing" begin
        v = bits(1234)[1:8]
        @test v == [0, 1, 0, 0, 1, 0, 1, 1]
        @test v isa Bits.BitVector1Mask
        v = bits(1.2)[10:17]
        @test v == [1, 0, 0, 1, 1, 0, 0, 1]
        @test v isa Bits.BitVector1Mask{Int64}
        @test all(bits(123)[[1, 2, 4, 5, 6, 7]])
        @test count(bits(123)) == 6
    end
end
