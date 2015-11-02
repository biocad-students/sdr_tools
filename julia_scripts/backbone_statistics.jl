require("ArgParse")
require("Iterators")
require("Requests")

using ArgParse, Iterators, Requests
import JSON
import GZip
import Requests

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


function loadFromRCSB(code :: String, destination_path :: String)
  if length(code) < 4
    return
  end
  pdb_file_name = string(destination_path, code, ".pdb")
  if isfile(pdb_file_name)
    #file already loaded, do nothing
    return
  end
  temp_file_name = string(destination_path, code, ".pdb.gz")
  res = get(string("http://www.rcsb.org/pdb/cgi/export.cgi/", code,
      ".pdb.gz?format=PDB&pdbId=", code, "&compression=gz"))
  if (statuscode(res) != 200)
    println(string("couldn't load PDB file, ID=", code))
    return
  end

  open(temp_file_name, "w") do gzfile
    write(gzfile, Requests.bytes(res))
  end

  gzstream = GZip.gzopen(temp_file_name)
    open(pdb_file_name, "w") do destFile
      write(destFile, readall(gzstream))
    end
  close(gzstream)
  rm(temp_file_name)
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
  segmentID :: String
  element :: String
  charge :: String
end

function parseAtomInfoFromString(line :: String)
  #TODO: check correctness
  #println(line)
  result = PDBAtomInfo(
      int(line[7:12]),
      strip(line[13:16]),
      line[17],
      strip(line[18:20]),
      line[22],
      int(line[23:26]),
      line[27],
      float(strip(line[31:38])),
      float(strip(line[39:46])),
      float(strip(line[47:54])),
      float(strip(line[55:60])),
      float(strip(line[61:66])),
      strip(line[73:76]),
      strip(line[77:78]),
      strip(line[79:80]))
  #println(result)
  result
end

immutable GeometryVector
  coordinates :: Array{Number, 1}
end
GeometryVector() = GeometryVector([0, 0, 0]) #by default is 3-dimensional

function GeometryVectorOp2(a :: GeometryVector, b :: GeometryVector, op)
  GeometryVector(map(op, zip(a.coordinates, b.coordinates)))
end

+(a :: GeometryVector, b :: GeometryVector) = GeometryVectorOp2(a, b, x-> x[1] + x[2])
-(a :: GeometryVector, b :: GeometryVector) = GeometryVectorOp2(a, b, x-> x[1] - x[2])
function /(a :: GeometryVector, b :: Number)
  GeometryVector(map(x -> x/b, a.coordinates))
end

function round2(a::GeometryVector)
  GeometryVector(map(x::Float64->round(x), a.coordinates))
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
  GeometryVector([v_x, v_y, v_z])
end

function cross3d(a :: GeometryVector, b :: GeometryVector)
  GeometryVector([
    a.coordinates[2]*b.coordinates[3] - a.coordinates[3]*b.coordinates[2],
    -(a.coordinates[1]*b.coordinates[3] - a.coordinates[3]*b.coordinates[1]),
    a.coordinates[1]*b.coordinates[2] - a.coordinates[2]*b.coordinates[1]
    ])
end
#println(normalize(GeometryVector([1,2,3])))

type Rotamer
  atoms :: Dict{String, GeometryVector}
  center :: GeometryVector
end
Rotamer() = Rotamer(Dict{String, GeometryVector}(), GeometryVector([0, 0, 0]))


function getPDBFileNames(input_file_name :: String, directory :: String = "files/")
  input_file = open(input_file_name, "r")
  result = String[]
  if (!isdir(directory))
    mkdir(directory)
  end
  while !eof(input_file)
    code = rstrip(readline(input_file))
    if !isfile(string(directory, code, ".pdb"))
      loadFromRCSB(code, directory)
    end
    if (length(code) > 0)
      push!(result, string(directory, code, ".pdb"))
    end
  end
  close(input_file)
  result
end

function readPDB(input_file_name :: String)
  records = Dict{Char, Dict{Int, Dict{String, PDBAtomInfo}}}()
  recordsAA = Dict{Char, Array{Int, 1}}()
  input_file = open(input_file_name, "r")
  while !eof(input_file)
    s = rstrip(readline(input_file), ['\r','\n'])
    if (length(s) < 4)
      continue
    end
    if s[1:4] == "ATOM"
      atom = parseAtomInfoFromString(s)
      if !(atom.resName in ["ALA", "ARG", "ASN", "ASP",
          "CYS", "GLN", "GLU", "GLY", "HIS", "ILE", "LEU", "LYS",
          "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL"])
          continue
      end
      if !(atom.chainID in keys(records))
        records[atom.chainID] = Dict{Int, Dict{String, PDBAtomInfo}}()
        recordsAA[atom.chainID] = Int[]
      end
      if !(atom.resSeq in keys(records[atom.chainID]))
        records[atom.chainID][atom.resSeq] = Dict{String, PDBAtomInfo}()
        push!(recordsAA[atom.chainID], atom.resSeq)
      end
      records[atom.chainID][atom.resSeq][atom.atom] = atom
      #push!(records, parseAtomInfoFromString(s))
    end
  end
  close(input_file)
  (records, recordsAA)
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

function calculateDistancesVect(v1, v2, v3)
  d1 = len(v1 + v2)
  d2 = len(v3 + v2)
  d3 = sign(cross3d(v1, v2)*v3) * len(v1 + v2 + v3)
  (d1, d2, d3)
end

typealias AtomPosition GeometryVector
typealias AminoacidInfo Dict{String, AtomPosition}

function getLocalVectors(v1, v2, v3, aminoacid)
  #get local coordinate system
  #i=2

  vp = v2 + v3
  x = normalize(cross3d(v2, vp))
  y = normalize(cross3d(vp, x))
  z = normalize(cross3d(x, y))
  vectors = AminoacidInfo()
  for (s, e) in [("CA", "C"), ("CA", "N"), ("CA", "O")]
    if haskey(aminoacid, e)
      vectors[e] = projectToAxes(
        getVector(aminoacid[e]) - getVector(aminoacid[s]),
        x, y, z)
    else
      vectors[e] = GeometryVector([0, 0, 0])
    end
  end
  sidechain = Rotamer()
  for e in keys(aminoacid)
    if !(e in ["CA", "C", "N", "O"])
      sidechain.atoms[e] = projectToAxes(
        getVector(aminoacid[e]) - getVector(aminoacid["CA"]),
        x, y, z)
    end
  end
  #println(map(x->GeometryVector([x[1], x[2], x[3]]), values(sidechain.atoms)))
  sidechainLength = length(values(sidechain.atoms))
  if (sidechainLength > 0)
    sidechain.center = sum(values(sidechain.atoms)) / sidechainLength
  end
  (x, y, z, aminoacid["CA"].resName, vectors, sidechain)
end

function getAverage(chainInfo :: Dict{String, Dict{(Int, Int, Int), Array{AminoacidInfo, 1}}})
    result = Dict{String, Dict{(Int, Int, Int), Dict{String, GeometryVector}}}()
    for (aa, aaInfo) in chainInfo
        result[aa] = Dict{(Int, Int, Int), Dict{String, GeometryVector}}()
        for (distances, positions) in aaInfo
            size = length(positions)
            result[aa][distances] = Dict{String, GeometryVector}()
            for x in positions
                for (k, v) in x
                    if !haskey(result[aa][distances], k)
                        result[aa][distances][k] = GeometryVector([0, 0, 0])
                    end
                    result[aa][distances][k] += v
                end
            end
            for k in keys(result[aa][distances])
                result[aa][distances][k] = result[aa][distances][k] / size
            end
        end
    end
    #result2 = Dict{String, Dict{(Int, Int, Int), Dict{String, Array{Number, 1}}}}()
    result2 = Dict{String, Dict{Int, Dict{Int, Dict{Int, Dict{String, GeometryVector}}}}}()
    for (aa, aaInfo) in result
        result2[aa] = Dict{Int, Dict{Int, Dict{Int, Dict{String, GeometryVector}}}}()
        for ((d1, d2, d3), positions) in aaInfo
            if !haskey(result2[aa], d1)
              result2[aa][d1] = Dict{Int, Dict{Int, Dict{String, GeometryVector}}}()
            end
            if !haskey(result2[aa][d1], d2)
              result2[aa][d1][d2] = Dict{Int, Dict{String, GeometryVector}}()
            end
            if !haskey(result2[aa][d1][d2], d3)
              result2[aa][d1][d2][d3] = Dict{String, GeometryVector}()
            end
            for (k, v) in positions
                result2[aa][d1][d2][d3][k] = v
            end
        end
    end
    result2
end


type RotamerInfo
  representatives :: Array{Rotamer, 1}
  total :: Int
  amounts :: Array{Int, 1}
end

RotamerInfo() = RotamerInfo(Rotamer[], 0, Int[])

#helper method.returns index of nearest rotamer group. or zero if no one is found
function findRotamerGroup(representatives :: Array{Rotamer, 1}, rotamer :: Rotamer, threshold :: Number = 1.7)
  for i in 1:length(representatives)
    if (len(representatives[i].center - rotamer.center) < threshold)
      return i
    end
  end
  0
end

function buildLibraryFragment(positions :: Array{Rotamer, 1})
  destination = RotamerInfo()
  #1. group by number of representatives
  # todo: when select greatest. if 1, take unaffected
  # at first - make it simple, but ugly
  if (length(positions)==0)
    println("got zero-length positions")
  end
  for rotamerGroup in sort(collect(groupby(r-> r.center, positions)), by=length, rev=true)
    if (length(rotamerGroup[1].atoms) == 0)
      #println("no atoms in rotamer group found")
      continue
    end
    i = findRotamerGroup(destination.representatives, rotamerGroup[1])
    if (i == 0)
      # add new rotamer to library
      push!(destination.representatives, rotamerGroup[1]) #add only 1 rotamer, they are the same
      push!(destination.amounts, length(rotamerGroup))
    else
      #add rotamer to existing cluster i
      destination.amounts[i] += length(rotamerGroup)
    end
    destination.total += length(rotamerGroup)
  end
  destination
end

function getRotamerDb(rotamers :: Dict{String, Dict{(Int, Int, Int), Array{Rotamer, 1}}})
    result = Dict{String, Dict{(Int, Int, Int), RotamerInfo}}()
    for (aa, aaInfo) in rotamers
        result[aa] = Dict{(Int, Int, Int), RotamerInfo}()
        for (distances, positions) in aaInfo
            size = length(positions)
            rotamerInfo = buildLibraryFragment(positions)
            if (length(rotamerInfo.representatives) > 0)
                result[aa][distances] = rotamerInfo
            end
        end
    end
    result2 = Dict{String, Dict{Int, Dict{Int, Dict{Int, RotamerInfo}}}}()
    for (aa, aaInfo) in result
      result2[aa] = Dict{Int, Dict{Int, Dict{Int, RotamerInfo}}}()
      for (dist, r) in aaInfo
        if !haskey(result2[aa], dist[1])
          result2[aa][dist[1]] = Dict{Int, Dict{Int, RotamerInfo}}()
        end
        if !haskey(result2[aa][dist[1]], dist[2])
          result2[aa][dist[1]][dist[2]] = Dict{Int, RotamerInfo}()
        end
        if !haskey(result2[aa][dist[1]][dist[2]], dist[3])
          result2[aa][dist[1]][dist[2]][dist[3]] = r
        end
      end
    end
    result2
end

function processChainPortion(aminoacids, meshSize = 0.3)
  v1 = getVector(aminoacids[2]["CA"]) - getVector(aminoacids[1]["CA"])
  v2 = getVector(aminoacids[3]["CA"]) - getVector(aminoacids[2]["CA"])
  v3 = getVector(aminoacids[4]["CA"]) - getVector(aminoacids[3]["CA"])
  processChainPortionVec(v1, v2, v3, aminoacid[2], meshSize)
end

function processChainPortionVec(v1, v2, v3, aminoacid, meshSize = 0.3)
  distances = map(x-> convert(Int, round(x/meshSize)), calculateDistancesVect(v1, v2, v3))
  (x, y, z, aa_name, vectors, sidechains) = getLocalVectors(v1, v2, v3, aminoacid)
  (distances, aa_name, vectors, sidechains)
end

function getVectorForSeq(sequence, ks, k, text_file_name, latticeSize = 1.22)
  if (length(ks)<4)
    println("length of keys <=4")
    println(text_file_name)
  end
  for i in [1, 2, 3, k-1, k, k + 1, k + 2, length(ks) - 1, length(ks)]
    if (i < 1 || i > length(ks))
      continue
    end

    if !haskey(sequence[ks[i]], "CA")
      println(sequence[ks[i]])
      println(text_file_name)
    end
  end
  if (k == 1)
    return (round2((getVector(sequence[ks[2]]["CA"]) - getVector(sequence[ks[1]]["CA"]))/latticeSize),
            round2((getVector(sequence[ks[2]]["CA"]) - getVector(sequence[ks[1]]["CA"]))/latticeSize),
            round2((getVector(sequence[ks[3]]["CA"]) - getVector(sequence[ks[2]]["CA"]))/latticeSize))
  end
  if (k == length(ks))
    return (round2((getVector(sequence[ks[length(ks)]]["CA"]) - getVector(sequence[ks[length(ks) - 1]]["CA"]))/latticeSize),
            round2((getVector(sequence[ks[length(ks) - 1]]["CA"]) - getVector(sequence[ks[length(ks) - 2]]["CA"]))/latticeSize),
            round2((getVector(sequence[ks[length(ks)]]["CA"]) - getVector(sequence[ks[length(ks) - 1]]["CA"]))/latticeSize))
  end

  if (k == length(ks) - 1)
    return (round2((getVector(sequence[ks[length(ks) - 1]]["CA"]) - getVector(sequence[ks[length(ks) - 2]]["CA"]))/latticeSize),
            round2((getVector(sequence[ks[length(ks)]]["CA"]) - getVector(sequence[ks[length(ks) - 1]]["CA"]))/latticeSize),
            round2((getVector(sequence[ks[length(ks) - 1]]["CA"]) - getVector(sequence[ks[length(ks) - 2]]["CA"]))/latticeSize))
  end

  return (round2((getVector(sequence[ks[k]]["CA"]) - getVector(sequence[ks[k - 1]]["CA"]))/latticeSize),
          round2((getVector(sequence[ks[k + 1]]["CA"]) - getVector(sequence[ks[k]]["CA"]))/latticeSize),
          round2((getVector(sequence[ks[k + 2]]["CA"]) - getVector(sequence[ks[k + 1]]["CA"]))/latticeSize))
end

function load_atom_info(text_file_name)
  basechainInfo = Dict{String, Dict{(Int, Int, Int), Array{AminoacidInfo, 1}}}()
  sidechainInfo = Dict{String, Dict{(Int, Int, Int), Array{Rotamer, 1}}}()
  pdb_file_names = getPDBFileNames(text_file_name)
  for pdb_file_name in pdb_file_names
    (atom_infos, atom_info_keys) = readPDB(pdb_file_name)
    for chain in keys(atom_infos)
      ks = atom_info_keys[chain]
      if (length(ks) <= 4)
        continue
      end
      for k in 1 : length(ks)
        (v1, v2, v3) = getVectorForSeq(atom_infos[chain], ks, k, pdb_file_name)
        (d, aa, b, s) = processChainPortionVec(v1, v2, v3, atom_infos[chain][ks[k]]) #[atom_infos[chain][i] for i in ks[k - width + 1 : k]])
        if !haskey(basechainInfo, aa)
          basechainInfo[aa] = Dict{(Int, Int, Int), Array{AminoacidInfo, 1}}()
          sidechainInfo[aa] = Dict{(Int, Int, Int), Array{Rotamer, 1}}()
        end
        if !haskey(basechainInfo[aa], d)
          basechainInfo[aa][d] = AminoacidInfo[]
          sidechainInfo[aa][d] = Rotamer[]
        end
        push!(basechainInfo[aa][d], b)
        push!(sidechainInfo[aa][d], s)
      end
    end
  end
  r1 = getAverage(basechainInfo)
  r2 = getRotamerDb(sidechainInfo)
  ( {"data" => r1, "meshSize" => 0.3, "latticeSize" => 1.22},
    {"data" => r2, "threshold" => 1.7, "meshSize"=> 0.3, "latticeSize" => 1.22})
end


function main()
    (r1, r2) = load_atom_info("list.txt")
    #parsed_args = parse_commandline()
    output_file = open("backbone.json", "w")
    println(output_file, JSON.json(r1, 1))
    close(output_file)
    output_file = open("sidechains.json", "w")
    println(output_file, JSON.json(r2, 1))
    close(output_file)
end

main()
