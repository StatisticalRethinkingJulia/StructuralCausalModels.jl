
Iterators.map(x -> x^2, 1:3) |> display
println()

collect(Iterators.map(x -> x^2, 1:3)) |> display

mapreduce(isodd, +, 1:5) |> display
