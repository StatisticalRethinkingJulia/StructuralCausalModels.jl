using StructuralCausalModels

ProjDir = @__DIR__
cd(ProjDir) #do

df = DataFrame!(CSV.File(scm_path("..", "data", "WaffleDivorce.csv"), delim=';'));
df = DataFrame(
  :s => df[:, :South],
  :a => df[:, :MedianAgeMarriage],
  :m => df[:, :Marriage],
  :w => df[:, :WaffleHouses],
  :d => df[:, :Divorce]
);

fname = scm_path("..", "examples", "SR", "SR6.4.3", "sr6.4.3.dot")
Sys.isapple() && run(`open -a GraphViz.app $(fname)`)

d = OrderedDict(
  :s => [:a, :m, :w],
  :a => [:m, :d],
  :m => [:d],
  :w => [:d]
);

dag = DAG("sr6.4.3", d; df=df);
show(dag)

adjustmentsets = adjustment_sets(dag, :w, :d)
println("Adjustment sets:\n")
adjustmentsets |> display
println()

function show_differences(res)
   if length(res) > 0
    println()
    res |> display
    println()
  end
end

debug = true

a = copy(dag.a)
ar = copy(a);
vars = names(a, 1)
zna = NamedArray(zeros(Int, size(a)), (vars, vars), ("Rows", "Cols"));

c = [:s]
m = Symbol[]

println()
s = StructuralCausalModels.update_s(a, c, debug)
#@assert s == [:s, :a, :m, :w, :d]

count = 0
while true
  local res
  global count += 1
  println("\ncount = $count\n")
  println()

  global ar
  at = copy(ar)
 
  println("\nupdate_21\n")
  ar21 = StructuralCausalModels.update_21(ar, m, debug);
  res = findall(ar21 .!== ar)
  show_differences(res)

  ar = copy(ar21);
  println("\nupdate_22\n")
  ar22 = StructuralCausalModels.update_22(ar, s, debug)
  res = findall(ar22 .!== zna)
  show_differences(res)

  ar .+= ar22

  println("\nupdate_23\n")
  ar23 = StructuralCausalModels.update_23(ar, m, debug)
  res = findall(ar23 .!== zna)
  show_differences(res)

  ar .+= ar23

  println("\nupdate_24\n")
  ar24 = StructuralCausalModels.update_24(ar, m, debug)
  res = findall(ar24 .!== zna)
  show_differences(res)

  ar .+= ar24

  println("\nupdate_25\n")
  ar25 = StructuralCausalModels.update_25(ar, m, debug)
  res = findall(ar25 .!== zna)
  show_differences(res)

  ar .+= ar25

  println("\nupdate_26\n")
  ar26 = StructuralCausalModels.update_26(ar, m, debug)
  res = findall(ar26 .!== zna)
  show_differences(res)

  ar .+= ar26

  println("\nupdate_27\n")
  ar27 = StructuralCausalModels.update_27(ar, s, debug)
  res = findall(ar27 .!== zna)
  show_differences(res)

  ar .+= ar27

  println("\nupdate_28\n")
  ar28 = StructuralCausalModels.update_28(ar, s, debug)
  res = findall(ar28 .!== zna)
  show_differences(res)

  ar .+= ar28

  println("\nupdate_29\n")
  ar29 = StructuralCausalModels.update_29(ar, m, debug)
  res = findall(ar29 .!== zna)
  show_differences(res)

  ar .+= ar29

  println("\nupdate_30\n")
  ar30 = StructuralCausalModels.update_30(ar, m, debug)
  res = findall(ar30 .!== zna)
  show_differences(res)

  ar .+= ar30

  ar == at && break

end

println("\nEnd of 1st while loop ($count iterations).\n")

ar_old = copy(ar)
println("\nupdate_a\n")
ar = StructuralCausalModels.update_a(ar, true)
res = findall(ar .!== ar_old)
show_differences(res)

ar_old = copy(ar)
println("\nupdate_b\n")
ar = StructuralCausalModels.update_b(ar, s, true)
res = findall(ar .!== ar_old)
show_differences(res)

an = copy(ar)
count = 0
while true
  global count += 1
  println("\ncount = $count\n")
  println()

  global ar
  global an
  global res
  at = copy(ar)

  println("\nupdate_27n\n")
  a27n = StructuralCausalModels.update_27n(ar, an)
  res = findall(a27n .!== zna)
  show_differences(res)

  an .+= a27n
  ar .+= a27n

  println("\nupdate_22n\n")
  a22n = StructuralCausalModels.update_22n(ar, an)
  res = findall(a22n .!== zna)
  show_differences(res)

  an .+= a22n
  ar .+= a22n

  at == ar && break

end

println("\nEnd of 2nd while loop ($count iterations).\n")

println("\nupdate_i\n")
ai = StructuralCausalModels.update_i(ar)
res = findall(ar .!== ai)
show_differences(res)

ar = copy(ai)

select = setdiff(vars, vcat(c, m))
println(select)
fr = ar[select, select]
println()

display(fr)
println()
