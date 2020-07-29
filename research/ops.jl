using StructuralCausalModels
import StructuralCausalModels.SymbolList


import Base.-

-(x::SymbolList, y::SymbolList) = (type=:line, x=x, y=y)

↔(x, y) = (type=:darrow, x=x, y=y)

→(x, y) = (type=:rarrow, x=x, y=y)

println(:b - :A)
println(:b ↔ [:A, :D])
println([:X, :T] → :A)

m = (:b - :A, [:X, :T] → :A, :b ↔ [:A, :D])

m[3] |> display
