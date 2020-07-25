using StructuralCausalModels
using GraphRecipes, Plots

ProjDir = @__DIR__
cd(ProjDir) #do

d_string = "dag {A -> {E Z}; B -> {D Z}; Z -> {D E}; E -> D}"

dag = DAG("conf_triangles", d_string);
show(dag)

graphplot(dag.e, names=names(dag.e, 1), curvature_scalar=0.1, nodesize=0.2,
  method=:spring, fontsize=10, arrow=1.0, nodeshape=:circle)

gui()
