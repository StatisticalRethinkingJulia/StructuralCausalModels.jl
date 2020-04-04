using StructuralCausalModels, RData, CSV

ProjDir = @__DIR__
cd(scm_path("..", "data")) #do

#  objs = load("marks.rda");
#  df = objs["marks"]
#  CSV.write("marks.csv", df; delim=',')

#  objs = load("derived.rda");
#  df = objs["derived"]["raw"];
#  CSV.write("derived.csv", df; delim=',')

#  objs = load("glucose.rda");
#  df = objs["glucose"];
#  CSV.write("glucose.csv", df; delim=',')

#end