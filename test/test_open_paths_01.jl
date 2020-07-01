using StructuralCausalModels, Test

ProjDir = @__DIR__
cd(ProjDir) #do

d = OrderedDict(
  :w => :s,
  :d => [:a, :w, :m],
  :m => [:a, :s],
  :a => :s
);

dag = DAG("test_open_paths_01", d);
fname = joinpath(ProjDir, "test_open_paths_01.dot")
to_graphviz(dag, fname)
#Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

to_dagitty(d) 

bs = basis_set(dag)
ap  = all_paths(dag, :w, :d)
bp = backdoor_paths(dag, ap, :w)

cs = Vector{Symbol}[]
syms = syms_in_paths(bp, :w, :d)

function adjsets(d::DAG, paths, syms)
  lsyms = deepcopy(syms)
  adjustment_sets = Vector{Symbol}[]
  for s in lsyms
    if sym_in_all_paths(bp, s)
      if length(open_paths(dag, bp, [s])) == 0
        #println("$s closes all paths")
        append!(adjustment_sets, [[s]])
        setdiff!(lsyms, [s])
      end
    end
  end
  len = 2
  local csyms
  while length(lsyms) >= len
    csyms = collect(combinations(lsyms, len))
    for s in csyms
      if length(open_paths(dag, bp, s)) == 0
        append!(adjustment_sets, [s])
        setdiff!(lsyms, s)
      end
    end
    len += 1
  end
  adjustment_sets
end

@testset "Open_path_01" begin

  adjs = adjsets(dag, bp, syms)
  @test adjs == adjustment_sets(dag, :w, :d)

end