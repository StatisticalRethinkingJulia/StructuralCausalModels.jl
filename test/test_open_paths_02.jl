using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

d_string = "dag {A -> {E Z}; B -> {D Z}; Z -> {D E}; E -> D}"
dag = DAG("test_open_paths_02", d_string);

@test to_dagitty(dag.d) == "dag { D <- E; D <- Z; E <- Z; D <- B; Z <- B; E <- A; Z <- A }"

bs = basis_set(dag)
@test bs[1].f == :B
@test bs[1].s == :A
@test bs[1].c == nothing
@test bs[2].f == :B
@test bs[2].s == :E
@test bs[2].c == [:A, :Z]
@test bs[3].f == :A
@test bs[3].s == :D
@test bs[3].c == [:B, :Z, :E]

fname = joinpath(ProjDir, "test_open_paths_02.dot")
to_graphviz(dag, fname)
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

ap  = all_paths(dag, :D, :E)
bp = backdoor_paths(dag, ap, :D)
adjs = adjustment_sets(dag, :E, :D)

@testset "Open_path_02" begin

  @test bp == [[:D, :Z, :E], [:D, :Z, :A, :E], [:D, :B, :Z, :E], [:D, :B, :Z, :A, :E]]
  @test adjs == [[:Z, :B], [:Z, :A]]

end