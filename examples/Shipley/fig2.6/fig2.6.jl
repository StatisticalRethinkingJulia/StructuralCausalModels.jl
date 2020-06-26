using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

fname = scm_path("..", "examples", "Shipley", "fig2.6", "fig2.6.dot")
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

# Read `=>` as `~` in regression models, or `<-` in causal models.
d = OrderedDict(
  :u => [:x, :v],
  :s1 => [:u],
  :w => [:v, :y],
  :s2 => [:w]
);

dag = DAG("fig2.6", d);
show(dag)

f = [:x]; s = [:v]
g1 = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) == $g1")
g1 = d_separation(dag, f, s; cset=[:u])
println("d_separation($(dag.name), $f, $s; cset=[:u]) == $g1")
g1 = d_separation(dag, f, s; cset=[:s1])
println("d_separation($(dag.name), $f, $s; cset=[:s1]) == $g1\n")

f = [:x]; s = [:w]
g2 = d_separation(dag, f, s)
println("d_separation($(dag.name), $(f), $s) == $g2")
g2 = d_separation(dag, f, s; cset=[:u])
println("d_separation($(dag.name), $f, $s; cset=[:u]) == $g2")
g2 = d_separation(dag, f, s; cset=[:v])
println("d_separation($(dag.name), $f, $s; cset=[:v]) == $g2")
g2 = d_separation(dag, f, s; cset=[:u, :v])
println("d_separation($(dag.name), $f, $s; cset=[:u, :v]) == $g2\n")

f = [:x]; s = [:y]
g3 = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) == $g3")
g3 = d_separation(dag, f, s; cset=[:u, :w])
println("d_separation($(dag.name), $f, $s; cset=[:u, :w]) == $g3")
g3 = d_separation(dag, f, s; cset=[:s1, :s2])
println("d_separation($(dag.name), $f, $s; cset=[:s1, :s2]) == $g3")
g3 = d_separation(dag, f, s; cset=[:u, :v, :w])
println("d_separation($(dag.name), $f, $s; cset=[:u, :v, :w]) == $g3")
g3 = d_separation(dag, f, s; cset=[:s1, :s2, :v])
println("d_separation($(dag.name), $f, $s; cset=[:s1, :s2, :v]) == $g3")
g3 = d_separation(dag, f, s; cset=[:s1])
println("d_separation($(dag.name), $f, $s; cset=[:s1]) == $g3\n")

f = [:u]; s = [:w]
g4 = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) == $g4"), [:s1]
g4 = d_separation(dag, f, s; cset=[:v])
println("d_separation($(dag.name), $f, $s; cset=[:v]) == $g4\n")

f = [:u]; s = [:w]
g4 = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) == $g4"), [:s1]
g4 = d_separation(dag, f, s; cset=[:v])
println("d_separation($(dag.name), $f, $s; cset=[:v]) == $g4\n")

bs = basis_set(dag)
bs |> display

adjustment_sets(dag, :u, :w) |> display

#end
