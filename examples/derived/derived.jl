# This example came from the R package ggm.

using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

derived_df = CSV.read(scm_path("..", "data", "derived.csv"));

df = DataFrame(
  :y => log.(derived_df.Sys ./ derived_df.Dia),
  :x => log.(derived_df.Dia),
  :z => derived_df.Wei ./ (derived_df.Hei / 100).^2,
  :w => derived_df.Age
  )

R_cov = "

           Sys       Dia        Age       Hei       Wei
Sys 176.691332 96.300211  71.738901 -2.415433  27.49471
Dia  96.300211 92.758985  54.571882 -4.371036  33.11839
Age  71.738901 54.571882 109.697146 -8.497357  60.60994
Hei  -2.415433 -4.371036  -8.497357 32.335624  24.83404
Wei  27.494715 33.118393  60.609937 24.834038 113.66808
";

# A DAG model with a latent variable U
# G = DAG(Y ~ Z + U, X ~ U + W, Z ~ W)
dag = DAG("ggm_derived", "DAG(Y ~ Z + U, X ~ U + W, Z ~ W)")

fname = ProjDir * "/derived.dot"
to_graphviz(dag, fname)
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

println("Definition of DAG:")
display(dag)

println("Adjacency matrix:")
display(dag.a)

println("ancestral graph:")
ag = ancestral_graph(dag; m=[:u])
display(ag)

println()
StructuralCausalModels.set_dag_df!(dag, df; force=true)
display(dag.s)

println()
bs = basis_set(dag)
display(bs)
