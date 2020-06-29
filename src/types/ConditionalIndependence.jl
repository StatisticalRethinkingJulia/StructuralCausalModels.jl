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
    println("  :$(ci.f) \u2210 :$(ci.s)")
  else
    println("  :$(ci.f) \u2210 :$(ci.s) | $(ci.c)")
   end 
end

show(io::IO, ci::ConditionalIndependency) = ci_show(io, ci)

export
  ConditionalIndependency