import Base: show, getindex, iterate, HasLength, HasEltype, length, sort



function sort(dag::DAG, bs::Vector{Vector{Symbol}})

  #topological_sort!(dag)

  dct = OrderedDict()
  for (i, s) in enumerate(dag.vars)
    dct[s] = i
  end

  # Arrange first sym < second sym

  for (i, s) in enumerate(bs)
    if isless(dct[s[2]], dct[s[1]])
      tmp = s[1]
      bs[i][1] = s[2]
      bs[i][2] = tmp
    end
  end
  fset = Symbol[]
  for (i, s) in enumerate(bs)
    push!(fset, s[1])
  end
  sort!(unique!(fset))
  bs2 = sort(bs; by = x -> x[1])

  # Now sort on second symbol within first symbol

  bs3 = Vector{Symbol}[]
  for s in fset
    indx = filter(x -> x[1] == s, bs2)
    sort!(indx; by = x -> dct[x[2]])
    for i in indx
      push!(bs3, i)
    end
  end
  bs3
end

function d_sep_combinations(dag::DAG, bs::Vector{Symbol})
  cs = Vector{Symbol}[Symbol[]]
  if length(bs) > 2
    for c in combinations(bs[3:end])
      push!(cs, c)
    end
  end
  bs_new = Vector{Symbol}[]
  for c in cs
    if length(c) == 0
      if d_separation(dag, bs[1], bs[2])
        push!(bs_new, [bs[1], bs[2]])
      end
    else
      if d_separation(dag, bs[1], bs[2]; c=c)
        push!(bs_new, [bs[1], bs[2], c...])
      end
    end
  end
  bs_new
end


struct BasisSet
  dag::DAG
  bs::Vector{Vector{Symbol}}

  # Inner constructor
  #BasisSet(dag, bs) = new(dag, sort(bs; by = x -> x[1]))

  
  function BasisSet(dag, bs)
    # Update,create and return the sorted BasisSet
    bs2 = sort(dag, bs)
    bs3 = Vector{Symbol}[]
    for b in bs2
      bs3 = vcat(bs3, d_sep_combinations(dag, b))
    end
    new(dag, bs3)
  end
  
end

iterate(b::BasisSet, state=1) =
  state > length(b.bs) ? nothing : (b.bs[state], state+1)

getindex(b::BasisSet, i::Int) = b.bs[i]

HasLength(b::BasisSet) = length(b.bs)

HasEltype(b::BasisSet) = eltype(b.bs)

length(b::BasisSet) = length(b.bs)

function bs_show(io::IO, bs::BasisSet)
  println("BasisSet[")
  for ci in bs.bs
      show(ConditionalIndependency(ci))
  end
  println("]")
end

show(io::IO, bs::BasisSet) = bs_show(io, bs)

export
  BasisSet