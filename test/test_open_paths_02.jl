using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir) #do

d_string = "dag {A -> {E Z}; B -> {D Z}; Z -> {D E}; E -> D}"
dag = DAG("test_open_paths_02", d_string);

@test to_dagitty(dag.d) == "dag { D <- E; D <- Z; E <- Z; D <- B; Z <- B; E <- A; Z <- A }"

bs = basis_set(dag)
@test bs[1][1] == :B
@test bs[1][2] == :A
@test bs[1][3:end] == Symbol[]
@test bs[2][1] == :D
@test bs[2][2] == :A
@test length(bs[2]) == 5
@test bs[3][1] == :E
@test bs[3][2] == :B
@test bs[3][3:end] == [:A, :Z]

#=
fname = joinpath(ProjDir, "test_open_paths_02.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)
=#

ap  = all_paths(dag, :D, :E)
bp = backdoor_paths(dag, ap, :D)
adjs = adjustment_sets(dag, :E, :D)

@testset "Open_path_02" begin

  @test bp == [[:D, :Z, :E], [:D, :Z, :A, :E], [:D, :B, :Z, :E], [:D, :B, :Z, :A, :E]]
  @test adjs == [[:Z, :B], [:Z, :A]]

end