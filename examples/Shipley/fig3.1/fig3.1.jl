using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir)

d = OrderedDict(
  :b => :a,
  [:c, :d] => :b,
  :e => [:c, :d],
  :e => :f
);

d = OrderedDict(
  :b => :a,
  [:c, :d] => :b,
  :e => [:c, :d, :f]
);

dag = DAG("fig_3_1", d)

fname = scm_path("..", "examples", "Shipley", "fig3.1", "fig3.1.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

ggm = to_ggm(d)
display(ggm)

bs = basis_set(dag)
display(bs)

adjustment_sets(dag, :c, :e) |> display
