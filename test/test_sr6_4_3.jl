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

d = OrderedDict(
  :s => [:a, :m, :w],
  :a => [:m, :d],
  :m => [:d],
  :w => [:d]
);
u = []

dag = DAG("sr6.4.3", d, df);
fname = scm_path("..", "examples", "SR", "SR6.4.3", "sr6.4.3.dot")
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

allpaths  = all_paths(dag, :w, :d)
backdoorpaths = backdoor_paths(dag, allpaths, :w)
openpaths = open_paths(dag, backdoorpaths)

adjustmentsets = adjustment_sets(dag, :w, :d)
