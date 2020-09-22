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

import Base.print
function print(io::IO, x::BasisSet)
  str = ""
  str *= "BasisSet["
  for c in bs.bs
    ci = ConditionalIndependency(c)
    if isnothing(ci.c)
      str *= "  :$(ci.f) \u2210 :$(ci.s)"
    else
      str *= "  :$(ci.f) \u2210 :$(ci.s) | $(ci.c)"
    end 
  end
  str *= "  ]"
  str 
end

function pluto_string(bs::BasisSet)
  str = ""
  str *= "BasisSet["
  for c in bs.bs
    ci = ConditionalIndependency(c)
    if isnothing(ci.c)
      str *= "  :$(ci.f) \u2210 :$(ci.s)"
    else
      str *= "  :$(ci.f) \u2210 :$(ci.s) | $(ci.c)"
    end 
  end
  str *= "  ]"
  str 
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
  ConditionalIndependency,
  print,
  pluto_string