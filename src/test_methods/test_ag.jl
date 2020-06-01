function test_ag(a::NamedArray; m=Symbol[], c=Symbol[], debug=true)

  function show_differences(res)
    if length(res) > 0
      println()
      res |> display
      println()
    end
  end

  vars = names(a, 1)
  zna = NamedArray(zeros(Int, size(a)), (vars, vars), ("Rows", "Cols"));

  s = StructuralCausalModels.update_s(a, c, debug)
  @assert s == [:n4, :n7, :n5, :n9, :n11, :n6]

  ar = copy(a);
  count = 0
  while true
    count += 1
    println("\ncount = $count\n")
    println()

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
    count += 1
    println("\ncount = $count\n")
    println()

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

  println("/ntest_ag() completed.")
  
  fr
end

function test_ag(d::DAG; m=Symbol[], c=Symbol[], debug=true)
  test_ag(d.a; m=m, c=c, debug=debug)
end



