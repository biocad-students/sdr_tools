require("ArgParse")

using ArgParse

push!(LOAD_PATH, dirname(@__FILE__()))

function parse_commandline()
    s = ArgParseSettings()

    @add_arg_table s begin
        "--score-matrix", "-s"
            help = "an option with an argument"
            arg_type = String
            #required = true
        "--clustering", "-c"
            help = "clustering type: N for neighbour joining, U for UPGMA, W for WPGMA"
            arg_type = String
            default = "N"
            #required = false
        "--verbose"
          help = "show debug messages while processing data"
          action = :store_true
        "fasta-file"
            help = "file in .fasta format with a set of protein strings"
            required = true
        "output-file"
            help = "file in .fasta format for saving results"
            required = true
    end

    return parse_args(s)
end

type PDBAtomInfo
  serial :: Int
  atom :: String
  altLoc :: Char
  resName :: String
  chainID :: Char
  resSeq :: Int
  iCode :: Char
  x :: Float64
  y :: Float64
  z :: Float64
  occupancy :: Float64
  tempFactor :: Float64
  element :: String
  charge :: String
end

function parseAtomInfoFromString(line :: String)
  #TODO: check correctness
  #println(line)
  result = PDBAtomInfo(
      int(line[6:12]),
      strip(line[13:16]),
      line[17],
      strip(line[17:20]),
      line[22],
      int(line[23:26]),
      line[27],
      float(strip(line[31:38])),
      float(strip(line[39:46])),
      float(strip(line[47:54])),
      float(strip(line[55:60])),
      float(strip(line[61:66])),
      strip(line[76:78]),
      strip(line[79:80]))
  #println(result)
  result
end

#immutable SlidingWindow
#    iter
#    width :: Int
#end

#function take(iter, n :: Int)
#  state = start(iter)
#  c = 1
#  result = Array{AbstractArray, 1}()
#  while !done(iter, state) && c < n
#    (i, state) = next(iter, state)
#    push!(result, i)
#    c += 1
#  end
#  result
#end

#Base.start(S::SlidingWindow) = take(S.iter, S.width)
#Base.next(S::SlidingWindow, state) = (take(S.iter, S.width), next(S.iter))
#Base.done(S::SlidingWindow, s) = length(s) >= S.width;

immutable GeometryVector
  coordinates :: Array{Number, 1}
end

function GeometryVectorOp2(a :: GeometryVector, b :: GeometryVector, op)
  GeometryVector(map(op, zip(a.coordinates, b.coordinates)))
end

+(a :: GeometryVector, b :: GeometryVector) = GeometryVectorOp2(a, b, x-> x[1] + x[2])
-(a :: GeometryVector, b :: GeometryVector) = GeometryVectorOp2(a, b, x-> x[1] - x[2])
function /(a :: GeometryVector, b :: Number)
  GeometryVector(map(x -> x/b, a.coordinates))
end
function *(a :: GeometryVector, b :: Number)
  GeometryVector(map(x -> x*b, a.coordinates))
end

function *(a :: GeometryVector, b :: GeometryVector)
  sum(map(x -> x[1] * x[2], zip(a.coordinates, b.coordinates)))
end

len(a :: GeometryVector) = sqrt(a*a)
#println(len(GeometryVector([1,2,3])))

normalize(a :: GeometryVector) = a/len(a)

function projection(projected :: GeometryVector, whereToProject :: GeometryVector)
  whereToProject*(projected*whereToProject)
end

function projectToAxes(v :: GeometryVector, x :: GeometryVector, y :: GeometryVector, z :: GeometryVector)
  v_x = v*x
  v_y = (v - x*v_x)*y
  v_z = (v - x*v_x - y*v_y)*z
  (v_x, v_y, v_z)
end

function cross3d(a :: GeometryVector, b :: GeometryVector)
  GeometryVector([
    a.coordinates[2]*b.coordinates[3] - a.coordinates[3]*b.coordinates[2],
    -(a.coordinates[1]*b.coordinates[3] - a.coordinates[3]*b.coordinates[1]),
    a.coordinates[1]*b.coordinates[2] - a.coordinates[2]*b.coordinates[1]
    ])
end
#println(normalize(GeometryVector([1,2,3])))

function readPDB(input_file_name :: String)
  records = Dict{Char, Dict{Int, Dict{String, PDBAtomInfo}}}()
  input_file = open(input_file_name, "r")
  while !eof(input_file)
    s = rstrip(readline(input_file), ['\r','\n'])
    if s[1:4] == "ATOM"
      atom = parseAtomInfoFromString(s)
      if !(atom.chainID in keys(records))
        records[atom.chainID] = Dict{Int, Dict{String, PDBAtomInfo}}()
      end
      if !(atom.resSeq in keys(records[atom.chainID]))
        records[atom.chainID][atom.resSeq] = Dict{String, PDBAtomInfo}()
      end
      records[atom.chainID][atom.resSeq][atom.atom] = atom
      #push!(records, parseAtomInfoFromString(s))
    end
  end
  records
end

getVector = a :: PDBAtomInfo -> GeometryVector([a.x, a.y, a.z])

#this function returns d_{i-1, i+1}, d_{i, i+2}, d_{i-1, i+2}
function calculateDistances(aminoacids)
  d1 = len(getVector(aminoacids[3]["CA"]) - getVector(aminoacids[1]["CA"]))
  d2 = len(getVector(aminoacids[4]["CA"]) - getVector(aminoacids[2]["CA"]))
  v1 = getVector(aminoacids[2]["CA"]) - getVector(aminoacids[1]["CA"])
  v2 = getVector(aminoacids[3]["CA"]) - getVector(aminoacids[2]["CA"])
  v3 = getVector(aminoacids[4]["CA"]) - getVector(aminoacids[3]["CA"])
  d3 = sign(cross3d(v1, v2)*v3) * len(v1 + v2 + v3)
  (d1, d2, d3)
end

AtomPosition = (Number, Number, Number)
AminoacidInfo = Dict{String, AtomPosition}

function getLocalVectors(aminoacids)
  #get local coordinate system
  #i=2
  v1 = getVector(aminoacids[3]["CA"]) - getVector(aminoacids[2]["CA"])
  vp = getVector(aminoacids[4]["CA"]) - getVector(aminoacids[2]["CA"])
  x = normalize(cross3d(v1, vp))
  y = normalize(cross3d(vp, x))
  z = normalize(cross3d(x, y))
  vectors = AminoacidInfo()
  for (s, e) in [("CA", "C"), ("CA", "N"), ("C", "O")]
    if haskey(aminoacids[2], e)
      vectors[string(s, "_", e)] = projectToAxes(
        getVector(aminoacids[2][e]) - getVector(aminoacids[2][s]),
        x, y, z)
    else
      vectors[string(s, "_", e)] = (0, 0, 0)
    end
  end
  sidechains = AminoacidInfo()
  for e in keys(aminoacids[2])
    if !(e in ["CA", "C", "N", "O"])
      sidechains[e] = projectToAxes(
        getVector(aminoacids[2][e]) - getVector(aminoacids[2]["CA"]),
        x, y, z)
    end
  end
  (x, y, z, aminoacids[2]["CA"].resName, vectors, sidechains)
end

function makeAverage(chainInfo :: Dict{String, Dict{(Int, Int, Int), Array{AminoacidInfo, 1}}})
  
end

function processChainPortion(aminoacids, meshSize = 0.3)
  distances =  map(x -> convert(Int, round(x / meshSize)), calculateDistances(aminoacids))
  (x, y, z, aa_name, vectors, sidechains) = getLocalVectors(aminoacids)
  (distances, aa_name, vectors, sidechains)
end

function load_atom_info(pdb_file_name)
  atom_infos = readPDB(pdb_file_name)
  basechainInfo = Dict{String, Dict{(Int, Int, Int), Array{AminoacidInfo, 1}}}()
  sidechainInfo = Dict{String, Dict{(Int, Int, Int), Array{AminoacidInfo, 1}}}()
  for chain in keys(atom_infos)
    width = 4
    ks = sort([k for k in keys(atom_infos[chain])])
    for k in width : length(ks)
      # ks[k-width+1: k]
      (d, aa, b, s) = processChainPortion([atom_infos[chain][i] for i in ks[k - width + 1 : k]])
      if !haskey(basechainInfo, aa)
        basechainInfo[aa] = Dict{(Int, Int, Int), Array{AminoacidInfo, 1}}()
        sidechainInfo[aa] = Dict{(Int, Int, Int), Array{AminoacidInfo, 1}}()
      end
      if !haskey(basechainInfo[aa], d)
        basechainInfo[aa][d] = AminoacidInfo[]
        sidechainInfo[aa][d] = AminoacidInfo[]
      end
      push!(basechainInfo[aa][d], b)
      push!(sidechainInfo[aa][d], s)
      #return
    end
  end
  println(makeAverage(basechainInfo))
  #println(makeAverage(sidechainInfo))
end

function main()
  load_atom_info("2OSL.pdb")
    #parsed_args = parse_commandline()

    #input_file = parsed_args["fasta-file"]
    #output_file = parsed_args["output-file"]
    #clustering = parsed_args["clustering"]
    #score_matrix_file = parsed_args["score-matrix"]
    #score_matrix = readMatrix(score_matrix_file)
    #println("Score matrix loaded...")

    #ProfileAligner.setScoringMatrix(score_matrix)
    #fasta_sequences = readSequences(input_file)
    #println("FASTA sequences loaded...")
    #seq = strToProfiles( fasta_sequences)
    #println("Sequences converted to profiles successfully, starting tree construction...")
    #result = (clustering == "N" ? NeighbourJoining(seq, scoreFunc, mergeFunc) :
    #  clustering == "W" ? WPGMA(seq, scoreFunc, mergeFunc) : UPGMA(seq, scoreFunc, mergeFunc) )
    #println("Tree constructed, writing to output_file...")
    #writeSequences(output_file, getstrings(result))
    #println("All done, go and do smthng else")
end

main()
