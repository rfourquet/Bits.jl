using Bits , Test


@testset "bitsize" begin
    for T in Base.BitInteger_types
        @test bitsize(T) === sizeof(T) * 8
        @test bitsize(zero(T)) === bitsize(one(T)) === bitsize(T)
    end
    @test bitsize(BigInt) === Bits.INF
    @test bitsize(Bool) === 1
    @test_throws MethodError bitsize(Float64)
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
end
