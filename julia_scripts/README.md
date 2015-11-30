Scripts description
===================

`backbone_statistics.jl`
------------------------

This script generates both backbone and rotamer libraries for full-atom model reconstruction.
Sample call:

```
julia backbone_statistics.jl -i list.txt -m 0.25 -b backbone.json -s sidechain.json
```
When gets called with no arguments, shows all available options and their meanings.

Input file (`-i` key) contains list of PDB IDs, one per line, which gets loaded to custom directory (which can be set with `-d` key).



`r14e12_converter.jl`
---------------------
helper script to convert portion of MCDP dataset to json.

`rotamer_library_generator.jl`
------------------------------
helper script to convert portion of MCDP dataset to json.
