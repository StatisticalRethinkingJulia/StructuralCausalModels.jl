# Structural Causal Models

To demonstrate the fuctionality in this package, setup of the 'marks' example from the ggm R package:
```julia
using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir)

df = DataFrame!(CSV.File(scm_path("..", "data", "marks.csv")));
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

dag = DAG("marks", d_string; df=df);
show(dag)
```

The `d_string` can also contain a dagitty causal model.
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

In this example a DataFrame with observed values has been provided and the related covariance matrix will be computed and stored in the DAG object:
```julia
display(dag.s)
```

```julia
5×5 Named Array{Float64,2}
Rows ╲ Cols │  :mechanics     :vectors     :algebra    :analysis  :statistics
────────────┼────────────────────────────────────────────────────────────────
:mechanics  │     305.768      127.223      101.579      106.273      117.405
:vectors    │     127.223      172.842      85.1573      94.6729       99.012
:algebra    │     101.579      85.1573      112.886      112.113      121.871
:analysis   │     106.273      94.6729      112.113       220.38      155.536
:statistics │     117.405       99.012      121.871      155.536      297.755
```

Additional DAG related functions are `adjacency_matrix()`, `edge_matrix()` and `dag_vars()`.

# Importing from and exporting to [dagitty.net](http://www.dagitty.net/dags.html#), dagitty and ggm (both are R packages)

Importing is easiest implicitly using the functions `from_dagitty()` and `from_ggm()` while constructing a DAG object, as shown above.

To export to dagitty.net, copy and paste the output from `to_dagitty()` into the `Model code` field on the dagitty.net web interface.

For both R packages, copy the output from `to_dagitty()` or `to_ggm()` to R.

# Adding a observations DataFrame or a covariance matrix 

Use `set_dag_df!()` and `set_dag_cov_matrix!()` for this. Note that if a DataFrame is added a covariance matrix is computed.

Although this initial version of StructuralCausalModels does not support latent variables yet, by using the keyword argument `force=true` no check is performed if all vertices/variables in the causal diagram are present in the DataFrame or covariance matrix.

# Directed separation

Given a causal graph `dag`, `d_separation(dag, f, l, cond)` determines if the vertices in set `f` are `d-separated` from the vertices in set `l` given the conditioning set `cond`.

Show several `d_separation` results for the marks model:
```julia
f = [:statistics]; s = [:mechanics];

e = d_separation(dag, f, s; cond=:algebra);
println("d_separation($(dag.name), $f, $s; cond=:algebra) = $e")

e = d_separation(dag, f, s);
println("d_separation($(dag.name), $f, $s) = $e")

print("d_separation($(dag.name), [:statistics], [:mechanics, :analysis]; cond=[:algebra]) = ");
println(d_separation(dag, [:statistics], [:mechanics, :analysis]; cond=[:algebra]))
```

will produce:
```julia
d_separation(marks, [:statistics], [:mechanics]; cond=:algebra) = false
d_separation(marks, [:statistics], [:mechanics]) = true
d_separation(marks, [:statistics], [:mechanics, :analysis]: cond=[:algebra]) = false
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

# Adjustment sets

The function  `basis_set()` provides a set of conditional independencies given the causal model. The conditioning set closes ("blocks the flow of causal info") all paths. The conditioning_set can be empty. It provides ways to test the chosen causal model given observational data.

The function `adjustment_sets()` answers a related question, i.e. how to prevent confounding in multiple regression models assuming the chosen causal model is correct.

Setup the WaffleDivorce example from StatisticalRethinking:
```julia
using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

df = DataFrame!(CSV.File(scm_path("..", "data", "WaffleDivorce.csv"), delim=';');
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
  [:a, :m, :w] => :s,
  :d => [:w, :m, :a],
  :m => [:a]
);
u = []

dag = DAG("sr6_4_3", d, df);
show(dag)

adjustmentsets = adjustment_sets(dag, :w, :d)
println("Adjustment sets for open paths: $(openpaths)\n")
adjustmentsets |> display

Adjustment sets:
2-element Array{Array{Symbol,1},1}:
 [:s]
 [:a, :m]
```

# Shipley test

Perform the Shipley test:
```julia
t = shipley_test(dag)
display(t)

(ctest = 2.816854003338401, df = 8, pv = 0.9453198036802164)
```

# Paths

Adjustment_sets is based on several path manipulations and checks:
```julia
allpaths  = all_paths(dag, :w, :d)
println("All paths between :w and :d:")
allpaths |> display
println()
```

for this [DAG](https://github.com/StatisticalRethinkingJulia/StructuralCausalModels.jl/blob/master/examples/SR/SR6.4.3/sr6.4.3.pdf) returns:
```julia
4-element Array{Array{Symbol,1},1}:
 [:w, :s, :a, :d]
 [:w, :s, :a, :m, :d]
 [:w, :s, :m, :d]
 [:w, :s, :m, :a, :d]
```

```julia
backdoorpaths = backdoor_paths(dag, allpaths, :w)
println("All backdoors between :w and :d:")
backdoorpaths |> display
println()

4-element Array{Array{Symbol,1},1}:
 [:w, :s, :a, :d]
 [:w, :s, :a, :m, :d]
 [:w, :s, :m, :d]
 [:w, :s, :m, :a, :d]

```
```julia
println("Show path: $(allpaths[2])")
show_dag_path(dag, allpaths[2]) |> display
println()

":w ⇐ :s ⇒ :a ⇒ :m ⇒ :d"
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
