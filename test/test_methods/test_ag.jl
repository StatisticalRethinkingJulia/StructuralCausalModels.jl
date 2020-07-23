function test_ag(amat::NamedArray; m=Symbol[], c=Symbol[], debug=false)

  function show_differences(res)
    if length(res) > 0
      debug && println()
      debug && display(res)
      debug && println()
    end
  end

  a = amat'
  vars = names(a, 1)
  zna = NamedArray(zeros(Int, size(a)), (vars, vars), ("Rows", "Cols"));

  s = StructuralCausalModels.update_s(a, c, debug)

  ar = copy(a);
  count = 0
  while true
    count += 1
    debug && println("\ncount = $count\n")
    debug && println()

    at = copy(ar)
   
    debug && println("\nupdate_21\n")
    ar21 = StructuralCausalModels.update_21(ar, m, debug);
    res = findall(ar21 .!== ar)
    debug && show_differences(res)

    ar = copy(ar21);
    debug && println("\nupdate_22\n")
    ar22 = StructuralCausalModels.update_22(ar, s, debug)
    res = findall(ar22 .!== zna)
    debug && show_differences(res)

    ar .+= ar22

    debug && println("\nupdate_23\n")
    ar23 = StructuralCausalModels.update_23(ar, m, debug)
    res = findall(ar23 .!== zna)
    debug && show_differences(res)

    ar .+= ar23

    debug && println("\nupdate_24\n")
    ar24 = StructuralCausalModels.update_24(ar, m, debug)
    res = findall(ar24 .!== zna)
    debug && show_differences(res)

    ar .+= ar24

    debug && println("\nupdate_25\n")
    ar25 = StructuralCausalModels.update_25(ar, m, debug)
    res = findall(ar25 .!== zna)
    debug && show_differences(res)

    ar .+= ar25

    debug && println("\nupdate_26\n")
    ar26 = StructuralCausalModels.update_26(ar, m, debug)
    res = findall(ar26 .!== zna)
    debug && show_differences(res)

    ar .+= ar26

    debug && println("\nupdate_27\n")
    ar27 = StructuralCausalModels.update_27(ar, s, debug)
    res = findall(ar27 .!== zna)
    debug && show_differences(res)

    ar .+= ar27

    debug && println("\nupdate_28\n")
    ar28 = StructuralCausalModels.update_28(ar, s, debug)
    res = findall(ar28 .!== zna)
    debug && show_differences(res)

    ar .+= ar28

    debug && println("\nupdate_29\n")
    ar29 = StructuralCausalModels.update_29(ar, m, debug)
    res = findall(ar29 .!== zna)
    debug && show_differences(res)

    ar .+= ar29

    debug && println("\nupdate_30\n")
    ar30 = StructuralCausalModels.update_30(ar, m, debug)
    res = findall(ar30 .!== zna)
    debug && show_differences(res)

    ar .+= ar30

    ar == at && break

  end

  debug && println("\nEnd of 1st while loop ($count iterations).\n")

  ar_old = copy(ar)
  debug && println("\nupdate_a\n")
  ar = StructuralCausalModels.update_a(ar, debug)
  res = findall(ar .!== ar_old)
  debug && show_differences(res)

  ar_old = copy(ar)
  debug && println("\nupdate_b\n")
  ar = StructuralCausalModels.update_b(ar, s, debug)
  res = findall(ar .!== ar_old)
  debug && show_differences(res)

  an = copy(ar)
  count = 0
  while true
    count += 1
    debug && println("\ncount = $count\n")
    debug && println()

    at = copy(ar)

    debug && println("\nupdate_27n\n")
    a27n = StructuralCausalModels.update_27n(ar, an)
    res = findall(a27n .!== zna)
    debug && show_differences(res)

    an .+= a27n
    ar .+= a27n

    debug && println("\nupdate_22n\n")
    a22n = StructuralCausalModels.update_22n(ar, an)
    res = findall(a22n .!== zna)
    debug && show_differences(res)

    an .+= a22n
    ar .+= a22n

    at == ar && break

  end

  debug && println("\nEnd of 2nd while loop ($count iterations).\n")

  debug && println("\nupdate_i\n")
  ai = StructuralCausalModels.update_i(ar)
  res = findall(ar .!== ai)
  debug && show_differences(res)

  ar = copy(ai)

  select = setdiff(vars, vcat(c, m))
  debug && println(select)
  fr = ar[select, select]

  #println("/ntest_ag() completed.")
  
  fr
end

function test_ag(d::DAG; m=Symbol[], c=Symbol[], debug=false)
  test_ag(d.e; m=m, c=c, debug=debug)
end
