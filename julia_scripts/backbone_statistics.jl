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



function readPDB(input_file_name :: String)
  records = Dict{Char, Dict{Int, Array{PDBAtomInfo, 1}}}()
  input_file = open(input_file_name, "r")
  while !eof(input_file)
    s = rstrip(readline(input_file), ['\r','\n'])
    if s[1:4] == "ATOM"
      atom = parseAtomInfoFromString(s)
      if !(atom.chainID in keys(records))
        records[atom.chainID] = Dict{Int, Array{PDBAtomInfo, 1}}()
      end
      if !(atom.resSeq in keys(records[atom.chainID]))
        records[atom.chainID][atom.resSeq] = PDBAtomInfo[]
      end
      push!(records[atom.chainID][atom.resSeq], atom)
      #push!(records, parseAtomInfoFromString(s))
    end
  end
  records
end

function load_atom_info(pdb_file_name)
  atom_infos = readPDB(pdb_file_name)
  for chain in keys(atom_infos)
    width = 4
    ks = [k for k in keys(atom_infos[chain])]
    for k in width : length(keys(atom_infos[chain]))
      println(ks[k-width+1: k])
    end
  end
  #println(atom_infos)
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
