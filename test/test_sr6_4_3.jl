using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

df = CSV.read(scm_path("..", "data", "WaffleDivorce.csv"), DataFrame);
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

dag = DAG("sr6.4.3", d; df=df);
fname = scm_path("..", "examples", "SR", "SR6.4.3", "sr6.4.3.dot")
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

allpaths  = all_paths(dag, :w, :d)
backdoorpaths = backdoor_paths(dag, allpaths, :w)
adjustmentsets = adjustment_sets(dag, :w, :d)

@testset "sr6_4_3" begin

  @test length(allpaths) == 4
  @test allpaths == [
    [:w, :s, :a, :d],
    [:w, :s, :a, :m, :d],
    [:w, :s, :m, :d],
    [:w, :s, :m, :a, :d]
  ]
  @test length(backdoorpaths) == 0
  @test length(adjustmentsets) == 0

end