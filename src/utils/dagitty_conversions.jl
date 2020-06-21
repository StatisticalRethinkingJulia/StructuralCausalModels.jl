function find_direction(s::AbstractString)
  isnothing(findfirst("<", s)) ? :rightarrow : :leftarrow
end

function replace_dag_term(s::AbstractString, from::Int, to::Int)
  term_vars = replace(s[from+1:to-1], " " => ",")
  "$(s[1:from-1])[$(term_vars)]$(s[to+1:end])"
end

function handle_term(s::AbstractString)
  ind = findfirst("[", s)
  if isnothing(ind)
    # Just a single Synbol
    return Symbol(s)
  else
    from = ind.start
    ind = findfirst("]", s)
    if isnothing(ind)
      @error "$s not well formed."
    end
    to = ind.start
    t = split(s[from+1:to-1], ",")
    return Symbol.(t)
  end
end

function split_and_insert!(f, t, s)
  new_t = [t[1]]
  for e in t[2:end-1]
    push!(new_t, e)
    push!(new_t, e)
  end
  push!(new_t, t[end])
  indx = sort(vcat(findall(">", s), findall("<", s)))
  @assert length(new_t) == 2length(indx)
  j = 1
  for i in 1:2:length(new_t)
    push!(f, "$(new_t[i]) $(s[indx[j].start]) $(new_t[i+1])")
    j += 1
  end
end

function from_dagitty(str::AbstractString; tolowercase=false)
  str = replace(str, "<-" => "<")
  str = replace(str, "->" => ">")
  startp = findall("{", str)
  endp = findall("}", str)
  f = split(str[startp[1].start+1:endp[end].stop-1], ";")

  dct = OrderedDict{SymbolList, SymbolList}()

  while length(f) > 0
    #display(f)
    s = pop!(f)
    dir = find_direction(s)
    #println(dir)

    ind = findfirst("{", s)
    while !isnothing(ind)
      from = ind.start
      to = findfirst("}", s)
      to = to.start
      s = replace_dag_term(s, from, to)
      ind = findfirst("{", s)
    end

    t = String.(replace(strip(s), " " => ""))
    t = split(t, x -> x in "><", keepempty=false)
    if length(t) > 2
      split_and_insert!(f, t, s)
      continue
    end

    term1 = handle_term(t[1])
    term2 = handle_term(t[2])

    if dir == :leftarrow
      #println([term1, dir, term2, term1 in keys((dct))])
      if term1 in keys(dct)
        #println(dct[term1])
        if typeof(dct[term1]) == Symbol
          #println(typeof(dct[term1]) == Symbol)
          dct[term1] = push!([dct[term1]], term2)
        else
          push!(dct[term1], term2)
        end
      else
        dct[term1] = term2
      end
    else
      if term2 in keys(dct)
        if typeof(dct[term2]) == Symbol
          dct[term2] = push!([dct[term2]], term1)
        else
          push!(dct[term2], term1)
        end
      else
        dct[term2] = term1
      end
    end
    #println(dct)
  end

  dct
end

function to_dagitty(d::OrderedDict; touppercase=false)
  str = "dag { "
  for (ind, key) in enumerate(keys(d))
    str = ind > 1 ? "$(str); " : "$(str)"
    rhs = d[key]
    if typeof(rhs) == Symbol
      rhs_str = String(rhs)
    elseif length(rhs) == 0
      # Not used yet
    elseif length(rhs) == 1
      rhs_str = String(rhs[1])
    elseif length(rhs) > 1
      rhs_str = "{ $(String(rhs[1]))"
      for s in rhs[2:end]
        rhs_str = "$(rhs_str) $(String(s))"
      end
      rhs_str = "$(rhs_str) }"
    end
    # Split multiple lhs symbols (if present)
    if typeof(key) == Symbol
      str = "$(str)$(String(key)) <- $rhs_str"
    elseif length(key) == 1
      str = "$(str)$(String(key[1])) <- $rhs_str"
    elseif length(key) > 1
      str = "$(str)$(String(key[1])) <- $(rhs_str); "
      for s in key[2:end-1]
        str = "$(str)$(String(s)) <- $(rhs_str)"
      end
      str = "$(str)$(String(key[end])) <- $(rhs_str)"
    end
  end 
  str = "$(str) }"
  touppercase ? uppercase(str) : str
end

function to_dagitty(d::DAG; touppercase=false)
  to_dagitty(d.d; touppercase=touppercase)
end

export
  from_dagitty,
  to_dagitty