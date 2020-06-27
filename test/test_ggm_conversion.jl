using StructuralCausalModels, Test

ggm_1 = " DAG(U ~X+V, S1~U, W~V+Y, S2~W, order=FALSE)";
ggm_2 = " DAG(U ~X+V, S1~U, W~V+Y, S2~W)";

@testset "ggm conversions" begin

  d1 = from_ggm(ggm_1)
  @test d1 == OrderedDict(
    :U  => [:X, :V],
    :S1 => :U,
    :W  => [:V, :Y],
    :S2 => :W
  )
  d2 = from_ggm(ggm_2)
  @test d2 == OrderedDict(
    :U  => [:X, :V],
    :S1 => :U,
    :W  => [:V, :Y],
    :S2 => :W
  )

  g1 = to_ggm(d1)
  @test g1 == "DAG(U ~ X + V, S1 ~ U, W ~ V + Y, S2 ~ W)"
  g2 = to_ggm(d2)
  @test g2 == "DAG(U ~ X + V, S1 ~ U, W ~ V + Y, S2 ~ W)"

  d3 =OrderedDict(
    [:u, :w] => :v,
    [:u] => :x,
    :s1 => [:u],
    :w => :y,
    [:s2] => [:w]
  );

  g3 = to_ggm(d3)
  @test g3 == "DAG(u ~ v, w ~ v, u ~ x, s1 ~ u, w ~ y, s2 ~ w)"

  g4 = to_ggm(d3, order=true)
  @test g4 == "DAG(u ~ v, w ~ v, u ~ x, s1 ~ u, w ~ y, s2 ~ w, order=TRUE)"

  dag = DAG("ggm_1", ggm_1)
  @test dag.d == OrderedDict(
    :U  => [:X, :V],
    :S1 => :U,
    :W  => [:V, :Y],
    :S2 => :W
  )

end
