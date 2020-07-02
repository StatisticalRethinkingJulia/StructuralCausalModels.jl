using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir) #do

d_string = "dag {k -> i; k -> D; m -> l; l -> k; n -> m; o -> n; p -> o; " *
  "q -> p; r -> q; r -> D; b -> a; " *
  "c -> b; f -> c; g -> f; h -> g; i -> h; a -> E; i -> E; E -> D}"

dag = DAG("test_open_paths_04", d_string);
bs = basis_set(dag)

adjs = adjustment_sets(dag, :E, :D)

fname = joinpath(ProjDir, "test_open_paths_04.dot")
to_graphviz(dag, fname)
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)


@testset "Open_path_04" begin

@test dag.d == OrderedDict(:D => [:E, :r, :k], :E => [:i, :a], 
  :h => :i, :g => :h, :f => :g, :c => :f, :b => :c, :a => :b, 
  :q => :r, :p => :q, :o => :p, :n => :o, :m => :n, :k => :l, 
  :l => :m, :i => :k)

@test length(bs) == 272
@test bs[3][1] == :D
@test bs[3][2] == :a
@test bs[3][3:end] == [:k, :E]
@test adjs == [[:i], [:k]]

end
