import Base: show, getindex, iterate, HasLength, HasEltype, length

struct ConditionalIndependency
  f::SymbolList
  s::SymbolList
  c::SymbolListOrNothing
end

ConditionalIndependency(f::SymbolList, s::SymbolList) =
  ConditionalIndependency(f, s, nothing)

function ConditionalIndependency(a::Array{Symbol,1})
  @assert length(a) > 1 "Length Symbol set too short."
  if length(a) == 2
    return ConditionalIndependency(a[1], a[2])
  end
  ConditionalIndependency(a[1], a[2], a[3:end])
end

function ci_show(io::IO, ci::ConditionalIndependency)
  if isnothing(ci.c)
    println("  $(ci.f) \u2210 $(ci.s)")
  else
    println("  $(ci.f) \u2210 $(ci.s) | $(ci.c)")
   end 
end

show(io::IO, ci::ConditionalIndependency) = ci_show(io, ci)

function append(c::ConditionalIndependency)
  v = [c.f, c.s]
  !isnothing(c.c) && length(c.c) > 0 && push!(v, c.c...)
  v
end

struct BasisSet
  bs::Vector{ConditionalIndependency}
end

function BasisSet(b::Array{Array{Symbol,1},1})
  BasisSet(ConditionalIndependency.(b))
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
      show(ci)
  end
  println("]")
end

show(io::IO, bs::BasisSet) = bs_show(io, bs)

export
  BasisSet,
  ConditionalIndependency