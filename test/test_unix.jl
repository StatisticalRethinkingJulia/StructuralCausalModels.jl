using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

d2 = OrderedDict(
  :AFF => [:ALN, :APA, :CDR],
  :AIS => [:AFF, :EGC, :SUS],
  :ALN => [:APA, :DET, :FTW, :PER, :SUS],
  :CDR => [:DET],
  :EGC => [:HOS],
  :FTW => [:DET, :EGC],
  :PER => [:DET],
  :SAN => [:AFF, :AIS, :ALN, :APA, :CDR],
  :SUS => [:EGC, :FTW, :HOS]
)

dag = DAG("unix", d2)

fname = joinpath(mktempdir(), "unix.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)
