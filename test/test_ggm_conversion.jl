using StructuralCausalModels
SymbolList = Union{Nothing, Symbol, Vector{Symbol}}

ggm_1 = " DAG(U ~X+V, S1~U, W~V+Y, S2~W, order=FALSE)";
ggm_2 = " DAG(U ~X+V, S1~U, W~V+Y, S2~W)";

d1 = from_ggm(ggm_1)
d1 |> display
d2 = from_ggm(ggm_2)
d2 |> display
println()

g1 = to_ggm(d1)
g1 |> display
g2 = to_ggm(d2)
g2 |> display

d3 =OrderedDict{SymbolList, SymbolList}(
  [:u, :w] => :v,
  [:u] => :x,
  :s1 => [:u],
  :w => :y,
  [:s2] => [:w]
)

g3 = to_ggm(d3)
g3 |> display

g4 = to_ggm(d3, order=true)
g4 |> display

dag = DAG("ggm_1", ggm_1)
dag |> display
