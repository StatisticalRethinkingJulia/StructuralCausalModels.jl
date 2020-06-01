using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

fname = scm_path("..", "examples", "Shipley", "fig2.6.a.dot")
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

# Read `=>` as `~` in regression models, or `<-` in causal models.
d = OrderedDict(
  :u => [:x, :v, :s1],
  :w => [:v, :y, :s2]
);

dag = DAG("fig2.6.a", d);
show(dag)

f = [:x]; s = [:v]
g1 = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) == $g1")
g1 = d_separation(dag, f, s, [:u])
println("d_separation($(dag.name), $f, $s, [:u) == $g1")
g1 = d_separation(dag, f, s, [:s1])
println("d_separation($(dag.name), $f, $s, [:s1]) == $g1\n")

f = [:x]; s = [:w]
g2 = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) == $g2")
g2 = d_separation(dag, f, s, [:u])
println("d_separation($(dag.name), $f, $s, [:u]) == $g2")
g2 = d_separation(dag, f, s, [:v])
println("d_separation($(dag.name), $f, $s, [:v]) == $g2")
g2 = d_separation(dag, f, s, [:u, :v])
println("d_separation($(dag.name), $f, $s, [:u, :v]) == $g2\n")

f = [:x]; s = [:y]
g3 = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) == $g3")
g3 = d_separation(dag, f, s, [:u, :w])
println("d_separation($(dag.name), $f, $s, [:u, :w]) == $g3")
g3 = d_separation(dag, f, s, [:s1, :s2])
println("d_separation($(dag.name), $f, $s, [:s1, :s2]) == $g3")
g3 = d_separation(dag, f, s, [:u, :v, :w])
println("d_separation($(dag.name), $f, $s, [:u, :v, :w]) == $g3")
g3 = d_separation(dag, f, s, [:s1, :s2, :u])
println("d_separation($(dag.name), $f, $s, [:s1, :s2, :u]) == $g3")
g3 = d_separation(dag, f, s, [:s1])
println("d_separation($(dag.name), $f, $s, [:s1]) == $g3\n")

f = [:u]; s = [:w]
g4 = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) == $g4"), [:s1]
g4 = d_separation(dag, f, s, [:v])
println("d_separation($(dag.name), $f, $s, [:v]) == $g4\n")

#end
