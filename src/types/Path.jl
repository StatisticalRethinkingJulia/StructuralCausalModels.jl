import Base: show

mutable struct Path
  ug::NamedArray
  f::Symbol
  l::Symbol
  path::Vector{Symbol}
  visited::Vector{Symbol}
  symbol_stack::Vector{Symbol}
end
Path(ug, f, l) = Path(
  ug,                                  # Undirected graph
  f,                                   # Path continuation symbol
  l,                                   # Path completed symbol
  Symbol[],                            # Symbols on path
  Symbol[],                            # Symbols already processed
  [f]                                  # Initial symbol
)

function path_show(io::IO, p::Path)
  println("\nPath object:")
  println(io, "(f = $(p.f), l = $(p.l))")
  println(io, "path = $(p.path)")
  println(io, "visited = $(p.visited)")
  println(io, "symbol_stack = $(p.symbol_stack)")
  println()
  display(p.ug)
  println()
end

show(io::IO, p::Path) = path_show(io, p)

export
  Path
