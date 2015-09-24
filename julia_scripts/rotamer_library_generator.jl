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
            required = false
        "--output-file"
            help = ".json file name form saving results"
            required = false
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

function readData(input_file)
  numbers = reduce(vcat, [map(x->float(split(strip(x))), readlines(input_file))])
  return reshape(numbers, 9, int(size(numbers)[1]/9))
end



function main()
    parsed_args = parse_commandline()


    #println(result)
    #output_file = open(parsed_args["output-file"], "w")
    #println(output_file, JSON.json(r1, 1))
    #close(output_file)
    #println(mapreduce(x->x[1]*x[2], +, zip(lr[1, :], lr[2, :])))
    #println(lr2[1, :])
    #println(unique([sort(map(abs, vec(res[i][1, :]))) for i = 1:4704]))
    #exit()
    signs = [[1 1 1]
      [1 1 -1]
      [1 -1 1]
      [-1 1 1]
      [1 -1 -1]
      [-1 1 -1]
      [-1 -1 1]
      [-1 -1 -1]]
    signs = [vec(signs[i, :]) for i = 1:8]
    vect = [[3 1 1],
      [3 1 0],
      [3 0 0],
      [2 2 1],
      [2 2 0]]
    vect = [vec(vect[i, :]) for i = 1:5]
    count = 0
    cc = 0
    vectors = reshape([collect(map(x-> x[1]*x[2], zip(signs[i], vect[j]))) for i = 1:8, j=1:5], 40,1)
    vectors = unique(mapreduce(x-> collect(permutations(x)), vcat, vectors))

    distances = Set()
    output_file = open(parsed_args["output-file"], "w")
    println(output_file, JSON.json(vectors))
    close(output_file)

    #println(size(vectors)) #should be 90 - ok
    for i in vectors
      for j in vectors

        #ff = i->i*1.0/sqrt(1.0*mapreduce(x->x[1]*x[2], +, zip(i, i)))
        minus = (i, j) -> map(x->x[1] - x[2], zip(i, j))
        len = i -> 1.0*mapreduce(x -> x[1] * x[2], + , zip(i, i))
        dist=(i, j)-> len(minus(i, j))
        v = 1.0*mapreduce(x->x[1]*x[2], +, zip(i,j))/sqrt(len(i)*len(j))
        if ((v <= round(cosd(72.5),1) && v >= round(cosd(154.0), 1)) && (dist(i, j)*1.22 > 4.05))
          push!(distances, dist(i, j))
        end
        #
        ##println(ff(i))
        #v = 1.0*mapreduce(x->x[1]*x[2], +, zip(i,j))/sqrt(len(i)*len(j))
        ##println([[i], [j], v])
        #if ((v <= round(cosd(73.5),1) && v >= round(cosd(154.0),1)) &&
        #  (dist(i, j)*1.22>4.05))
        #  #println(dist(i, j)*1.22)
        #  #println("ok")
        #  println(v)
        #  count += 1
        #else
        #  #println("not ok")
        #  cc+=1
        #end
      end
    end
    println(sort([x for x in distances]))
    #println(cc)
end

main()
