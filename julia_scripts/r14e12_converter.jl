require("ArgParse")
require("Iterators")

using ArgParse, Iterators
import JSON
push!(LOAD_PATH, dirname(@__FILE__()))

function parse_commandline()
    s = ArgParseSettings()

    @add_arg_table s begin
        "--E-ONE", "-o"
            help = "converter for e-one energy term"
            action = :store_true
            #required = true
        "--input-file"
            help = "file with E-one values"
            required = true
        "--output-file"
            help = ".json file name form saving results"
            required = true
    end

    return parse_args(s)
end
aaNames = ["GLY", "ALA", "SER", "CYS",
  "VAL", "THR", "ILE", "PRO",
  "MET", "ASP", "ASN", "LEU",
  "LYS", "GLU", "GLN", "ARG",
  "HIS", "PHE", "TYR", "TRP",
  "CYX"]
aaIndices = Dict(collect(zip(aaNames, 1:21)))

immutable E12Data
  aa1 :: String
  aa2 :: String
  n14 :: Int
  e214 :: Array{Number, 1}
end

function process_r14aa12(result :: Dict{String, Dict{String, E12Data}})
  {"data"=>
  [
  aa1 =>
    [
      aa2 => result[aa1][aa2]
      for aa2 in intersect(aaNames, keys(result[aa1]))
    ]
    for aa1 in intersect(aaNames, keys(result))
    ],
    "startIndex" => -86,
    "endIndex" => 91,
    "binRightBorders" => [-79, -55, -35, -25, -12, 14, 26, 46, 65, 72, 81]
    }
end

function readE12part(input_file)
  (aa1, aa2, n14) = split(rstrip(readline(input_file)))
  numbers = Number[]
  while(!eof(input_file))
    s = readline(input_file)
    if beginswith(s, "=")
      break
    end
    append!(numbers, map(x->float(x), split(rstrip(s))))
  end
  return (aa1, aa2, int(n14), numbers)
end



function main()
    parsed_args = parse_commandline()
    input_file = open(parsed_args["input-file"], "r")
    result = Dict{String, Dict{String, E12Data}}()
    while !eof(input_file)
      (aa1, aa2, n14, numbers) = readE12part(input_file)
      if !haskey(result, aa1)
        result[aa1] = Dict{String, E12Data}()
      end
      result[aa1][aa2] = E12Data(aa1, aa2, n14, numbers)
      #push!(result, rstrip(readline(input_file)))
    end
    close(input_file)
    r1 = process_r14aa12(result)
    output_file = open(parsed_args["output-file"], "w")
    println(output_file, JSON.json(r1, 1))
    close(output_file)
end

main()
