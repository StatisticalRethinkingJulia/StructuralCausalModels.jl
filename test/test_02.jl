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

fname = scm_path("..", "examples", "SCM", "SR6.4.3", "sr6.4.3.dot")
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

adjustmentsets = adjustment_sets(dag, :w, :d)
println("Adjustment sets for open paths: $(openpaths)\n")
adjustmentsets |> display
