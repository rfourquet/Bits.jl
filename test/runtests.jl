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
