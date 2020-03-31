using StructuralCausalModels, RData, CSV

ProjDir = @__DIR__
cd(scm_path("..", "data")) do

  objs = load("marks.rda");
  df = objs["marks"]

  CSV.write("marks.csv", df; delim=',')

  objs = load("derived.rda");
  df = objs["derived"]["raw"];

  CSV.write("derived.csv", df; delim=',')

  objs = load("anger.rda");
  df = objs["anger"]

  #CSV.write("anger_cov.csv", df; delim=',')

end