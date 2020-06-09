using StructuralCausalModels

# fig2.6.dag <- dagitty("dag { {X V} -> U; S1 <- U; {Y V} -> W; W -> S2}")
dag_1 = "dag { U <- {X V}; S1 <- U; {Y V} -> W; S2 <- W}";
dag_2 = "dag { S1 <- U <- {X V}; {Y V} -> W -> S2}";
dag_3 = "dag { S1 <- U; U <- X; U <- V; Y -> W; V -> W -> S2}";

str=dag_1
d1 = from_dagitty(str)
d1 |> display
println()

str=dag_2
d2 = from_dagitty(str)
d2 |> display
println()

str=dag_3
d3 = from_dagitty(str)
d3 |> display
println()

g1 = to_dagitty(d1)
g1 |> display

g2 = to_dagitty(d2)
g2 |> display

g3 = to_dagitty(d3)
g3 |> display

d4 =OrderedDict(
  [:u, :w] => :v,
  [:u] => :x,
  :s1 => [:u],
  :w => :y,
  [:s2] => [:w]
)

g4 = to_dagitty(d4)
g4 |> display

dag = DAG("dag_1", dag_1)
dag |> display
