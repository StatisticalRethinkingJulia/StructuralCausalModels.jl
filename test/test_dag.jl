using Revise
using StructuralCausalModels

#=
import StructuralCausalModels: dag_vars, edge_matrix, SymbolList


function dag_vars(d::OrderedDict{SymbolList, SymbolList})
  vars = Symbol[]
  for var in keys(d)
    if isnothing(var)
      @warn "LHS can't be ampty set: $var."
    elseif typeof(var) == Symbol
      append!(vars, [var])
      handle_rhs!(vars, d[var])
    elseif typeof(var) == Vector{Symbol}
      append!(vars, var)
      handle_rhs!(vars, d[var])
    end
  end
  unique(vars)
end

function handle_rhs!(vars::Vector{Symbol}, rhs::SymbolList)
  if isnothing(var)
    append!(vars, [])
  elseif typeof(rhs) == Symbol
    append!(vars, [rhs])
  elseif typeof(rhs) == Vector{Symbol}
    append!(vars, rhs)
  end
end


function edge_matrix(d::OrderedDict{SymbolList, SymbolList})
  vars = dag_vars(d)
  l = length(vars)
  a = zeros(Int, l, l)

  for key in keys(d)
    println([1, key, d[key]])
    if typeof(key) == Symbol
      println([2, key])
      for (ind, var) in enumerate(vars)
        if var == key
          println([7, var, ind, key])
          for (i, vr) in enumerate(vars)
            if typeof(d[key]) == Symbol
              a[ind, i] += vr == d[key] ? 1 : 0
              println([8, ind, i, d[key], vr==d[key], a[ind, i]])
            elseif typeof(d[key]) == Vector{Symbol}
              if vr in d[key]
                a[ind, i] += 1
                println([9, ind, i, d[key], vr in d[key], a[ind, i]])
              end
            end
          end
        end
      end
    elseif typeof(key) == Vector{Symbol}
      println([3, key])
      for (ind, var) in enumerate(vars)
        if var in key
          println([4, var, ind, var in key])
          for (i, vr) in enumerate(vars)
            if typeof(d[key]) == Symbol
              a[ind, i] += vr == d[key] ? 1 : 0
              println([5, ind, i, d[key], vr==d[key], a[ind, i]])
            elseif typeof(d[key]) == Vector{Symbol}
              if vr in d[key]
                a[ind, i] += 1
                println([6, ind, i, d[key], vr in d[key], a[ind, i]])
              end
            end
          end
        end
      end
    end
    println(NamedArray(Int.(a), (vars, vars), ("Rows", "Cols")))
  end
  NamedArray(Int.(a), (vars, vars), ("Rows", "Cols"))
end
=#

d =OrderedDict(
  [:u, :w] => :v,
  [:u] => :x,
  :s1 => [:u],
  :w => :y,
  [:s2] => [:w]
)

e = StructuralCausalModels.edge_matrix(d)
display(e)

d1 = OrderedDict(
  :u => [:x, :v],
  :s1 => [:u],
  :w => [:v, :y],
  :s2 => [:w]
);

e1 = StructuralCausalModels.edge_matrix(d1)
display(e1)

#=
7×7 Named Array{Int64,2}
Rows ╲ Cols │  :u   :x   :v  :s1   :w   :y  :s2
────────────┼──────────────────────────────────
:u          │   0    1    1    0    0    0    0
:x          │   0    0    0    0    0    0    0
:v          │   0    0    0    0    0    0    0
:s1         │   1    0    0    0    0    0    0
:w          │   0    0    1    0    0    1    0
:y          │   0    0    0    0    0    0    0
:s2         │   0    0    0    0    1    0    0
=#