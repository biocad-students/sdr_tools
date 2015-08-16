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

function parseAtomInfoFromString(str :: String)
  #TODO: add parser and check correctness
end

function readPDB(input_file_name :: String)
  records = PDBAtomInfo[]
  input_file = open(input_file_name, "r")
  while !eof(input_file)
    s = rstrip(readline(input_file), ['\r','\n'])
    if s[1:4] == "ATOM"
      push!(records, parseAtomInfoFromString(s))
    end
  end
  records
end

function load_atom_info(pdb_file_name)
  atom_infos = readPDB(pdb_file_name)
  
end

function main()
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
