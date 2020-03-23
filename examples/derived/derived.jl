using RData, Statistics

objs = load("/Users/rob/Projects/R/ggm/data/derived.rda");

df = objs["derived"]["raw"];

# cov(derived$raw)

R_cov = "

           Sys       Dia        Age       Hei       Wei
Sys 176.691332 96.300211  71.738901 -2.415433  27.49471
Dia  96.300211 92.758985  54.571882 -4.371036  33.11839
Age  71.738901 54.571882 109.697146 -8.497357  60.60994
Hei  -2.415433 -4.371036  -8.497357 32.335624  24.83404
Wei  27.494715 33.118393  60.609937 24.834038 113.66808
";

cov_m = cov(Array(df))
cov_m |> display
println()
cor_m = cor(Array(df))
cor_m |> display

# Adjacency matrix

R_adj_m = "
   y1 y2 y6 y3 y5 y4
y1  0  0  0  0  0  0
y2  1  0  0  0  0  0
y6  1  0  0  1  0  1
y3  0  1  0  0  0  0
y5  0  0  0  1  0  1
y4  0  0  0  0  0  0
"

# Edge matrix

R_edge_m = "
edgematrix(d)
   y1 y2 y6 y3 y5 y4
y1  1  1  1  0  0  0
y2  0  1  0  1  0  0
y6  0  0  1  0  0  0
y3  0  0  1  1  1  0
y5  0  0  0  0  1  0
y4  0  0  1  0  1  1

"