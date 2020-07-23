using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

d_string = "dag {A -> {E Z}; B -> {D Z}; Z -> {D E}; E -> D}"

dag = DAG("conf_triangles", d_string);
show(dag)

to_ggm(dag) |> display
println()

fname = ProjDir * "/conf_triangles.dot"
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

println("Basis set:")
bs = basis_set(dag)
display(bs)
#=
E ⊥ B | A, Z
D ⊥ A | B, E, Z
A ⊥ B
=#

f = :A; s = :B;

e = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) = $e\n")

f = [:B]; s = [:E];

e = d_separation(dag, f, s; c=[:A, :Z])
println("d_separation($(dag.name), $f, $s; cond=[:A, :Z]) = $e\n")

ap = all_paths(dag, :E, :D)
ap |> display
println()

bp = backdoor_paths(dag, ap, :E)
bp |> display
println()

adjustmentsets = adjustment_sets(dag, :E, :D)
println("Adjustment sets:")
adjustmentsets |> display

#end

