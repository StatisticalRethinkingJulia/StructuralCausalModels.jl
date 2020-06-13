using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

m5_1_2_1 = OrderedDict(
  :d => [:a, :m],
  :m => :a
);

# DMA_dag1:

dag5_1_2_1 = DAG("sr5_1_2_1", m5_1_2_1);
show(dag5_1_2_1)

fname = joinpath(mktempdir(), "m5.1.2.1.dot")
to_graphviz(dag5_1_2_1, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

bs = basis_set(dag5_1_2_1)
display(bs)

adjustmentsets = adjustment_sets(dag5_1_2_1, :a, :d)
println("Adjustment sets:")
adjustmentsets |> display

# DMA_dag2:

m5_1_2_2 = OrderedDict(
  [:d, :m] => :a
);

dag5_1_2_2 = DAG("sr5_1_2_12", m5_1_2_2);
show(dag5_1_2_1)

fname = joinpath(mktempdir(), "m5.1.2.2.dot")
to_graphviz(dag5_1_2_2, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

bs = basis_set(dag5_1_2_2)
display(bs)

adjustmentsets = adjustment_sets(dag5_1_2_2, :a, :d)
println("Adjustment sets:")
adjustmentsets |> display

#end
