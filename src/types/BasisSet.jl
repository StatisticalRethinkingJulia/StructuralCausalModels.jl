import Base: show, getindex, iterate, HasLength, HasEltype, length

struct BasisSet
  bs::Vector{Vector{Symbol}}
  BasisSet(bs) = new(sort(bs; by = x -> x[1]))
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