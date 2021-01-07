using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir)

df = CSV.read(scm_path("..", "data", "WaffleDivorce.csv"), DataFrame);
df = DataFrame(
  :s => df[:, :South],
  :a => df[:, :MedianAgeMarriage],
  :m => df[:, :Marriage],
  :w => df[:, :WaffleHouses],
  :d => df[:, :Divorce]
);

fname = scm_path("..", "examples", "SR", "SR6.4.3", "sr6.4.3.dot")
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

d = OrderedDict(
  :s => [:a, :m, :w],
  :a => [:m, :d],
  :m => [:d],
  :w => [:d]
);

dag = DAG("sr6.4.3", d, df=df);

@testset "sr6_4_3b" begin
  
  adjustmentsets = adjustment_sets(dag, :w, :d)
  @test adjustmentsets == Array{Symbol,1}[]

  c = [:s]
  ag = ancestral_graph(dag; c=c)
  @test names(ag, 1) == [:a, :m, :w, :d]
  @test Array(ag) == [
    0  10  10  10;
   10   0  10  10;
   10  10   0  10;
   10  10  10   0
  ]

end