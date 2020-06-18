using StructuralCausalModels
using GraphRecipes, Plots

include("/Users/rob/.julia/dev/StructuralCausalModels/examples/dagitty/confounding_triangle.jl");

graphplot(dag.e, names=names(dag.e, 1), curvature_scalar=0.1, nodesize=0.5,
  method=:spring, fontsize=10, arrow=1.0)

gui()
