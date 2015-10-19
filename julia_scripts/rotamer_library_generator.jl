require("ArgParse")
require("Iterators")

using ArgParse, Iterators
import JSON
push!(LOAD_PATH, dirname(@__FILE__()))

aa3letter = {
  "A" => "ALA",
  "R" => "ARG",
  "N" => "ASN",
  "D" => "ASP",
  "C" => "CYS",
  "Q" => "GLN",
  "E" => "GLU",
  "G" => "GLY",
  "H" => "HIS",
  "I" => "ILE",
  "L" => "LEU",
  "K" => "LYS",
  "M" => "MET",
  "F" => "PHE",
  "P" => "PRO",
  "S" => "SER",
  "T" => "THR",
  "W" => "TRP",
  "Y" => "TYR",
  "V" => "VAL"
}

function parse_commandline()
    s = ArgParseSettings()

    @add_arg_table s begin
        "--E-ONE", "-o"
            help = "converter for e-one energy term"
            action = :store_true
            #required = true
        "--input-file-phi"
            help = "file with rotamer angles"
            required = true
        "--input-file-energies"
            help = "file with rotamer energies"
            required = false
        "--output-file"
            help = ".json file name for saving results"
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

function readRotamerLibraryInfo(angles_input_file_name, energies_input_file_name)
  angles_file = open(angles_input_file_name, "r")
  #println(readlines(input_file)[0])
  angles = Dict{Int, Array{Float64, 1}}()
  while !eof(angles_file)
    #println(readline(input_file))
    s = readline(angles_file)
    if (int(split(s)[2]) != 0)
      angles[int(split(s)[1])] = map(float, split(readline(angles_file)))
    end
    #return
  end
  close(angles_file)

  energies_file = open(energies_input_file_name, "r")
  by_aa = Dict{String, Dict{Int, Array{Float64, 1}}}()
  while !eof(energies_file)
    info = split(readline(energies_file))
    ibin = int(info[2])
    aa = aa3letter[info[1]]
    bdeg = int(info[4])
    if !haskey(by_aa, aa)
      by_aa[aa] = Dict{Int, Array{Float64, 1}}()
    end
    if int(info[3]) == 1
      by_aa[aa][ibin] = [
        - log(float(readline(energies_file))*bdeg) for i = 1 : bdeg
          ]
    else
      for i = 1:int(info[4])
        readline(energies_file)
      end
    end
  end
  close(energies_file)

  {"phi" => angles, "energies" => by_aa}
end

function main()
    parsed_args = parse_commandline()
    data = readRotamerLibraryInfo(parsed_args["input-file-phi"], parsed_args["input-file-energies"])
    #println(data)
    output_file = open(parsed_args["output-file"], "w")
    println(output_file, JSON.json(data, 1))
    close(output_file)
end

main()
