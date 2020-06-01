ProjDir = @__DIR__
cd(ProjDir)

open(`$(nodejs_cmd())`, "w", stdout) do io
  for i in 1:4
    println(io, "console.log('Hi #$i');")
  end
  println(io, "console.log('Hi #5');")
  println(io, "a = 6")
  println(io, "console.log(a);")
  #close(io)
end

mutable struct Pipes
    gstdin :: Pipe
    gstdout :: Pipe
    gstderr :: Pipe
    Pipes() = new()
end

const P = Pipes()

# initialize gnuplot
function init_node()
  global P
  try
      success(`$(nodejs_cmd()) --version`)
  catch
      error("NodeJS cannot be initialized.")
  end
  gstdin = Pipe()
  gstdout = Pipe()
  gstderr = Pipe()
  gproc = run(pipeline(`$(nodejs_cmd())`,
    stdin = gstdin, stdout = gstdout, stderr = gstderr),
    wait = false)
  process_running(gproc) || error("There was a problem starting up nodejs.")
  close(gstdout.in)
  close(gstderr.in)
  close(gstdin.out)
  P.gstdin = gstdin
  P.gstdout = gstdout
  P.gstderr = gstderr

  return nothing
end

# write commands to gnuplot's pipe
function nodejs_send(s)
    config[:debug] && println(s)  # print gnuplot commands if debug enabled
    w = write(P.gstdin, string(s,"\n"))
    # check that data was accepted by the pipe
    if !(w > 0)
        println("Something went wrong writing to gnuplot STDIN.")
        return
    end
    flush(P.gstdin)
end

## Copyright (c) 2013 Miguel Bazdresch
##
## This file is distributed under the 2-clause BSD License.

# Asynchronously reads the IO and buffers the read content. When end marker
# (GastonDone, which is stored in `gmarker` to account for Unix/Windows
# variability in line endings) is found in the content, sends everything
# between start (GastonBegin) and end markers to the returned channel.
# In case of timeout sends :timeout; in case of end of file, sends :eof.

function async_reader(io::IO, timeout_sec)::Channel
    ch = Channel(1)
    task = @async begin
        reader_task = current_task()
        function timeout_cb(timer)
            put!(ch, :timeout)
            Base.throwto(reader_task, InterruptException())
        end

        buf = ""
        while (match_done = findfirst(gmarker_done, buf)) == nothing
            timeout = Timer(timeout_cb, timeout_sec)
            data = String(readavailable(io))
            if data == ""; put!(ch, :eof); return; end
            timeout_sec > 0 && close(timeout) # Cancel the timeout
            buf *= data
        end
        match_begin = findfirst(gmarker_start, buf)
        start = (match_begin != nothing) ? last(match_begin)+1 : 1
        put!(ch, buf[start:first(match_done)-1])
    end
    bind(ch, task)
    return ch
end

