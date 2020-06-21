using StructuralCausalModels, Test

d1 =OrderedDict(
  [:u, :w] => :v,
  [:u] => :x,
  :s1 => [:u],
  :w => :y,
  [:s2] => [:w]
)

e1 = StructuralCausalModels.edge_matrix(d1)

d2 = OrderedDict(
  :u => [:x, :v],
  :s1 => [:u],
  :w => [:v, :y],
  :s2 => [:w]
);

e2 = StructuralCausalModels.edge_matrix(d2)

@testset "dag_formulations" begin

  syms = names(e1, 1)
  for s1 in syms
    for s2 in syms
      @test e1[s1, s2] == e2[s1, s2]
    end
  end

end