# Structural Causal Models

To demonstrate the fuctionality in this package, setup of the 'marks' example from the ggm R package:
```julia
using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir)

df = CSV.read(scm_path("..", "data", "marks.csv"));
```

# DAG - directed acyclic graphs

`DAG()` accepts either an OrderedDict, an `adjacency_matrix` or a ggm/dagitty string.
See the `ancestral_graph` section below for an example using an `adjacency_matrix`.

Below `d_string` holds a ggm DAG definition.

```julia
d_string = "DAG(
    mechanics ~ vectors+algebra, 
    vectors ~ algebra, 
    statistics ~ algebra+analysis, 
    analysis ~ algebra)"

dag = DAG("marks", d_string, df);
show(dag)
```

The `d_string` could also contain a dagitty causal model.
```julia
# fig2.6.dag <- dagitty("dag { {X V} -> U; S1 <- U; {Y V} -> W; S2 <- W}")
dag = DAG("fig_2_6", "dag {{X V} -> U; S1 <- U; {Y V} -> W; S2 <- W}")
```

In the REPL `show(dag)` will display:
```julia
DAG object:

name = "marks"
vars = [:mechanics, :vectors, :algebra, :statistics, :analysis]

OrderedDict{Symbol,Union{Nothing, Array{Symbol,1}, Symbol}} with 4 entries:
  :mechanics  => [:vectors, :algebra]
  :vectors    => :algebra
  :statistics => [:algebra, :analysis]
  :analysis   => :algebra
```

Internally, a DAG object will always contain an OrderedDict representation of the DAG. This representation is used in all functions. In the definition of the OrderedDict, read `=>` as `~` in regression models or `<-` in causal models.


Optional display the DAG using GraphViz:
```julia
fname = ProjDir * "/marks.dot"
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)
```

The DAG pdf is [here](https://github.com/StatisticalRethinkingJulia/StructuralCausalModels.jl/blob/master/docs/src/marks.pdf).

In this case a DataFrame with observed values has been provided and the related covariance matrix has been computed and stored in the DAG object:
```julia
display(dag.s)

5×5 Named Array{Float64,2}
Rows ╲ Cols │  :mechanics     :vectors     :algebra    :analysis  :statistics
────────────┼────────────────────────────────────────────────────────────────
:mechanics  │     305.768      127.223      101.579      106.273      117.405
:vectors    │     127.223      172.842      85.1573      94.6729       99.012
:algebra    │     101.579      85.1573      112.886      112.113      121.871
:analysis   │     106.273      94.6729      112.113       220.38      155.536
:statistics │     117.405       99.012      121.871      155.536      297.755

```

Additional DAG related functions are `adjacency_matrix()`, `edge_matrix()`, `to_ggm()`, `from_ggm()`, `to_dagitty()`, `from_dagitty()`, `set_dag_df!()` and `set_dag_cov_matrix!()`.

# D_separation

Given a causal graph, `d_separation(dag, f, l, cond)` determines if the vertices in set `f` are `d-separated` from the vertices in set `l` given the conditioning set `cond`.

Show several `d_separation` results for the marks model:
```julia
f = [:statistics]; s = [:mechanics]; sel = vcat(f, s)
cond = [:algebra]

e = d_separation(dag, f, s, cond)
println("d_separation($(dag.name), $f, $s, $cond) = $e")

e = d_separation(dag, f, s)
println("d_separation($(dag.name), $f, $s) = $e")

print("d_separation($(dag.name), [:statistics], [:mechanics], [:vectors]) = ")
println(d_separation(dag, [:statistics], [:mechanics], [:vectors]))

print("d_separation($(dag.name), [:statistics], [:mechanics], [:analysis, :vectors]) = ")
println(d_separation(dag, [:statistics], [:mechanics], [:analysis, :vectors]))

print("d_separation($(dag.name), [:statistics, :analysis], [:mechanics], [:algebra]) = ")
println(d_separation(dag, [:statistics, :analysis], [:mechanics], [:algebra]))

print("d_separation($(dag.name), [:statistics], [:mechanics, :vectors], [:algebra]) = ")
println(d_separation(dag, [:statistics], [:mechanics, :vectors], [:algebra]))

print("d_separation($(dag.name), [:statistics], [:mechanics, :analysis], [:algebra]) = ")
println(d_separation(dag, [:statistics], [:mechanics, :analysis], [:algebra]))

print("d_separation($(dag.name), [:analysis], [:vectors]) = ")
println(d_separation(dag, [:analysis], [:vectors]))

print("d_separation($(dag.name), [:analysis], [:vectors], [:algebra]) = ")
println(d_separation(dag, [:analysis], [:vectors], [:algebra]))

print("d_separation($(dag.name), [:vectors], [:statistics], [:algebra]) = ")
println(d_separation(dag, [:analysis], [:vectors], [:algebra]))

print("d_separation($(dag.name), [:statistics], [:algebra], [:analysis]) = ")
println(d_separation(dag, [:statistics], [:algebra], [:analysis]))

print("d_separation($(dag.name), [:statistics, :analysis], [:mechanics, :vectors]) = ")
println(d_separation(dag, [:statistics, :analysis], [:mechanics, :vectors]))

print("d_separation($(dag.name), [:statistics, :analysis], [:mechanics, :vectors], [:algebra]) = ")
println(d_separation(dag, [:statistics, :analysis], [:mechanics, :vectors], [:algebra]))
```

will produce:
```julia
d_separation(marks, [:statistics], [:mechanics], [:algebra]) = false
d_separation(marks, [:statistics], [:mechanics]) = true
d_separation(marks, [:statistics], [:mechanics], [:vectors]) = true
d_separation(marks, [:statistics], [:mechanics], [:analysis, :vectors]) = true
d_separation(marks, [:statistics, :analysis], [:mechanics], [:algebra]) = false
d_separation(marks, [:statistics], [:mechanics, :vectors], [:algebra]) = false
d_separation(marks, [:statistics], [:mechanics, :analysis], [:algebra]) = false
d_separation(marks, [:analysis], [:vectors]) = true
d_separation(marks, [:analysis], [:vectors], [:algebra]) = false
d_separation(marks, [:vectors], [:statistics], [:algebra]) = false
d_separation(marks, [:statistics], [:algebra], [:analysis]) = false
d_separation(marks, [:statistics, :analysis], [:mechanics, :vectors]) = true
d_separation(marks, [:statistics, :analysis], [:mechanics, :vectors], [:algebra]) = false
```

# Basis set

A minimal set of `d_separation` statements is called a `basis_set`.

A `basis_set` is not necessarily unique but it is sufficient to predict the complete set of `d_separation` statements.

Compute the basis_set:
```julia
bs = basis_set(dag)
display(bs)

BasisSet[
  vectors ∐ analysis | [:algebra]
  vectors ∐ statistics | [:algebra, :analysis]
  mechanics ∐ analysis | [:algebra, :vectors]
  mechanics ∐ statistics | [:algebra, :vectors, :analysis]
]
```

# Shipley test

Perform the Shipley test:
```julia
t = shipley_test(dag)
display(t)

(ctest = 2.816854003338401, df = 8, pv = 0.9453198036802164)
```

# Adjustment sets

Setup the WaffleDivorce example from StatisticalRethinking:
```julia
using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

df = CSV.read(scm_path("..", "data", "WaffleDivorce.csv"), delim=';');
df = DataFrame(
  :s => df[:, :South],
  :a => df[:, :MedianAgeMarriage],
  :m => df[:, :Marriage],
  :w => df[:, :WaffleHouses],
  :d => df[:, :Divorce]
);

fname = scm_path("..", "examples", "SR", "SR6.4.3", "sr6.4.3.dot")
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

d = OrderedDict(
  :s => [:a, :m, :w],
  :a => [:m, :d],
  :m => [:d],
  :w => [:d]
);
u = []

dag = DAG("sr6.4.3", d, df);
show(dag)

adjustmentsets = adjustment_sets(dag, :w, :d)
println("Adjustment sets for open paths: $(openpaths)\n")
adjustmentsets |> display

Adjustment sets:
2-element Array{Array{Symbol,1},1}:
 [:s]
 [:a, :m]
```

# Paths

Adjustment_sets is based on several path manipulations and checks:
```julia
allpaths  = all_paths(dag, :w, :d)
println("All paths between :x and :y:")
allpaths |> display
println()

backdoorpaths = backdoor_paths(dag, allpaths, :w)
println("All backdoors between :w and :d:")
backdoorpaths |> display
println()

openpaths = open_paths(dag, backdoorpaths)
println("All open (backdoor) paths between :w and :d:")
openpaths |> display
println()

println("Show path: $(allpaths[2])")
show_dag_path(dag, allpaths[2]) |> display
println()
```

# Ancestral graph

Setup an ancestral_graph example:
```julia
using StructuralCausalModels, Test

ProjDir = @__DIR__

#include(scm_path("test_methods", "test_ag.jl"))

amat_data = transpose(reshape([
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
  0,0,0,0,1,0,1,0,1,1,0,0,0,0,0,0,
  1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
  0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0
], (16,16)));

vars = [Symbol("n$i") for i in 1:size(amat_data, 1)]
a = NamedArray(Int.(amat_data), (vars, vars), ("Rows", "Cols"));

dag = DAG("ag_example", a)

m = [:n3, :n5, :n6, :n15, :n16];
c = [:n4, :n7];

fr = StructuralCausalModels.test_ag(a; m=m, c=c)

fr1 = ancestral_graph(a; m=m, c=c)
@test all(fr .== fr1)

fr2 = StructuralCausalModels.test_ag(dag.a; m=m, c=c)
@test all(fr .== fr2);

println()
display(fr)
println()
```
