function from_ggm(d::AbstractString; tolowercase=false)
  str = replace(strip(d), " " => "")
  str = replace(str, "\n" => "")
  startp = findall("(", str)
  endp = findall(")", str)
  orderp = findall("order", str)
  if length(orderp) == 0
    f = split(str[startp[1].start+1:endp[end].stop-1], ",")
  else
    f = split(str[startp[1].start+1:orderp[1].start-2], ",")
  end
  dct = OrderedDict{Symbol, SymbolList}()
  for s in f
    if tolowercase
      lhs = Symbol(lowercase(split(s, "~")[1]))
      rhs = Symbol.(lowercase.(split(split(s, "~")[2], "+")))
    else
      lhs = Symbol(split(s, "~")[1])
      rhs = Symbol.(split(split(s, "~")[2], "+"))
    end  
    rhs = length(rhs) == 1 ? rhs[1] : rhs
    dct[lhs] = rhs
  end
  dct
end 

function to_ggm(d::OrderedDict; touppercase=false, order=false)
  str = "DAG("
  for (ind, key) in enumerate(keys(d))
    str = ind > 1 ? "$(str), " : "$(str)"
    rhs = d[key]
    if typeof(rhs) == Symbol
      rhs_str = String(rhs)
    elseif length(rhs) == 0
      # Not used yet
    elseif length(rhs) == 1
      rhs_str = String(rhs[1])
    elseif length(rhs) > 1
      rhs_str = "$(String(rhs[1]))"
      for s in rhs[2:end]
        rhs_str = "$(rhs_str) + $(String(s))"
      end
    end
    # Split multiple lhs symbols (if present)
    if typeof(key) == Symbol
      str = "$(str)$(String(key)) ~ $rhs_str"
    elseif length(key) == 1
      str = "$(str)$(String(key[1])) ~ $rhs_str"
    elseif length(key) > 1
      str = "$(str)$(String(key[1])) ~ $(rhs_str), "
      for s in key[2:end-1]
        str = "$(str)$(String(s)) ~ $(rhs_str)"
      end
      str = "$(str)$(String(key[end])) ~ $(rhs_str)"
    end
  end 
  str = "$(str))"
  str = touppercase ? uppercase(str) : str
  order ? "$(str[1:end-1]), order=TRUE)" : "$(str)"
end

function to_ggm(d::DAG; touppercase=false, order=false)
  to_ggm(d.d; touppercase=touppercase, order=order)
end

export
  to_ggm,
  from_ggm