using StructuralCausalModels

# fig2.6.dag <- dagitty("dag { {X V} -> U; S1 <- U; {Y V} -> W; W -> S2}")
dag_1 = "dag { U <- {X V}; S1 <- U; {Y V} -> W; S2 <- W}";
dag_2 = "dag { S1 <- U <- {X V}; {Y V} -> W -> S2}";
dag_3 = "dag { S1 <- U; U <- X; U <- V; Y -> W; V -> W -> S2}";

d1 = from_dagitty(dag_1);
d2 = from_dagitty(dag_2);
d3 = from_dagitty(dag_3);
g1 = to_dagitty(d1);
g2 = to_dagitty(d2);
g3 = to_dagitty(d3)

d4 =OrderedDict(
  [:u, :w] => :v,
  [:u] => :x,
  :s1 => [:u],
  :w => :y,
  [:s2] => [:w]
)
g4 = to_dagitty(d4)

dag = DAG("dag_1", dag_1)

@testset "dagitty conversions" begin

  @test d1 == OrderedDict(
    :S2 => :W,
    :W  => [:Y, :V],
    :S1 => :U,
    :U  => [:X, :V]
  )
  @test d2 == OrderedDict(
    :S2 => :W,
    :W  => [:Y, :V],
    :U  => [:X, :V],
    :S1 => :U
  )
  @test d3 == OrderedDict(
    :S2 => :W,
    :W  => [:V, :Y],
    :U  => [:V, :X],
    :S1 => :U
  )
  @test g1 == "dag { S2 <- W; W <- { Y V }; S1 <- U; U <- { X V } }"
  @test g2 == "dag { S2 <- W; W <- { Y V }; U <- { X V }; S1 <- U }"
  @test g3 == "dag { S2 <- W; W <- { V Y }; U <- { V X }; S1 <- U }"
  @test g4 == "dag { u <- v; w <- v; u <- x; s1 <- u; w <- y; s2 <- w }"
  @test dag.d == OrderedDict(
    :S2 => :W,
    :W  => [:Y, :V],
    :S1 => :U,
    :U  => [:X, :V]
  )

end
