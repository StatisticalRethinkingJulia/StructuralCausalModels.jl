using Revise
using StructuralCausalModels
using Combinatorics

ProjDir = @__DIR__
cd(ProjDir) #do

function syms_in_paths(paths, f, l)
  thepaths = deepcopy(paths)
  syms = Symbol[]
  for p in thepaths
    setdiff!(p, [f, l])
    append!(syms, p)
    unique!(syms)
  end
  syms
end

function sym_in_all_paths(paths, sym)
  all([sym in p for p in paths])
end

d_string = "dag {A -> {E Z}; B -> {D Z}; Z -> {D E}; E -> D}"
dag = DAG("test_open_paths_02", d_string);
show(dag)

to_dagitty(dag.d) |> display

bs = basis_set(dag)
bs |> display

fname = joinpath(ProjDir, "test_open_paths_02.dot")
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

ap  = all_paths(dag, :D, :E)
bp = backdoor_paths(dag, ap, :D)
bp |> display

cs = Vector{Symbol}[]
syms = syms_in_paths(bp, :w, :d)
#syms |> display

function adjsets(d::DAG, paths, syms)
  lsyms = deepcopy(syms)
  adjustment_sets = Vector{Symbol}[]
  for s in lsyms
    if sym_in_all_paths(bp, s)
      if length(open_paths(dag, bp, [s])) == 0
        println("$s closes all paths")
        append!(adjustment_sets, [[s]])
        setdiff!(lsyms, [s])
      end
    end
  end
  lsyms |> display
  len = 2
  local csyms
  while length(lsyms) >= len
    csyms = collect(combinations(lsyms, len))
    csyms |> display
    for s in csyms
      if length(open_paths(dag, bp, s)) == 0
        println("$s closes all paths")
        append!(adjustment_sets, [s])
        setdiff!(lsyms, s)
      end
    end
    len += 1
  end
  lsyms |> display

  adjustment_sets
end

println()
adjs = adjsets(dag, bp, syms)
adjs |> display