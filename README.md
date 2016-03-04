# sdr_tools

[![Build Status](https://travis-ci.org/biocad/sdr_tools.svg?branch=master)](https://travis-ci.org/biocad/sdr_tools)

This repo originally was supposed to contain code for finding specificity determining regions (in Scala). But now it contains folding algorithm implementation+qhull implementation. Original idea was given by my scientific advisor and it is still unclear, the name of the repo left unchanged.

Here are files & code in Scala I promised to write.

Currently is repo is not actively maintained.

There is still much to do - to move from folding to alascan, to refactor code, etc. But no one needs it.

License
===================

MIT

Remarks
===================

Some of todos can be found in `scala_code/README.md` (in russian). There is also option to add specificity determining residues guessing algorithm to old code in python (branch `python_proto`).

Branch `bond-restore` has slightly different implementation of backbone reconstruction: it restores atoms laying on `bond`, not belonging to aminoacid. Worths mention: bond atoms are counted for aminoacid type closer to C-terminus (because I looked at proline and thought that aminoacid would be reconstructed better in that case). Anyway, with that type of reconstruction appearance of blank intervals, closer atoms triangles is still possible. [Feig et al.,2000] states that this type of reconstruction errors is typical for lattice models. That's why I decided not to merge that reconstruction to master branch, but decided to keep it there.

Additional references
=======================

All references used in algorithm implementation are given in scala_code/README.md. Some additional are:

http://www.actabp.pl/pdf/2_2004/349s.pdf

Robert L. Jernigan. Protein folds. Curr.Opinion in Struct.Biology, 1992, 2:248-256:
"In the final stage of the folding protocol, the entire full atom structures may be reconstructed"

Skolnick, J., Kolinski, A., Brooks, C.L., 111, Godzik, A. A method for prediction of protein structure from sequence. Current Biol. 3:414-423, 1993. -

julia_scripts
======================

It turned out that there is no information on pdb backbone vectors statistics, at least I couldn't find any. Method of obtaining such information is well-defined - that's why I added helper script for collecting such information from protein data bank files (in folder julia_scripts).
