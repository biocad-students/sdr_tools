# sdr_tools

This repo originally was supposed to contain code for finding specificity determining regions (with draft code in Scala).

I planned to use this repo for my thesis scripts, and decided to open MS_thesis instead, and to use this repo for saving files & code in Scala I promised to write.
I'll put final code version after refactoring to zmactep/ig-toolkit, this repo will contain some additional benchmarks and test scripts.

I wrote folding implementation, qhull-based tesselation code, and now I decided to officially give up.

There is still much to do - to move from folding to alascan, to refactor code... to kill zombie hidden in [[ru.biocad.ig.common.algorithms.geometry.QHull]], et cetera, et cetera, et cetera, but...

[I OFFICIALLY GIVE UP. I mean it.](http://frontalot.com/media.php/531/MC_Frontalot_-_Zero_Day_-_03._Jacquelyn_Hyde_%5BTEASER%5D.mp3)

Final remarks
===================

Some of todos can be found in `scala_code/README.md` (in russian). There is also option to add specificity determining residues guessing algorithm to old code in python (branch `python_proto`).

QHull.scala is a huge, ugly, undead zombie who killed me. There is a comparision-with-zero weapon he used against me. I think there is a chance that it can be destroyed, but I'm already dead. There is an option to take smaller EPSILON value (my tests passed).

Other implementations of tesselation should be forgotten.

Branch `bond-restore` has slightly different implementation of backbone reconstruction: it restores atoms laying on `bond`, not belonging to aminoacid. Worths mention: bond atoms are counted for aminoacid type closer to C-terminus (because I looked at proline and thought that aminoacid would be reconstructed better in that case). Anyway, with that type of reconstruction appearance of blank intervals, closer atoms triangles is still possible. [Feig et al.,2000] states that this type of reconstruction errors is typical for lattice models. That's why I decided not to merge that reconstruction to master branch, but decided to keep it there.

Additional references
=======================

All references  used in algorithm implementation are given in scala_code/README.md. Some additional are:

http://www.actabp.pl/pdf/2_2004/349s.pdf

Robert L. Jernigan. Protein folds. Curr.Opinion in Struct.Biology, 1992, 2:248-256:
"In the final stage of the folding protocol, the entire full atom structures may be reconstructed"

Skolnick, J., Kolinski, A., Brooks, C.L., 111, Godzik, A. A method for prediction of protein structure from sequence. Current Biol. 3:414-423, 1993. -

details on full-atom model reconstruction
==================================================

Michael Feig, Piotr Rotkiewicz, Andrzej Kolinski, Jeffrey Skolnick and Charles L. Brooks III.
Accurate Reconstruction of All-Atom Protein Representations From Side-Chain-Based Low-Resolution Models

julia_scripts
======================

It turned out that there is no information on pdb backbone vectors statistics, at least I couldn't find any. Method of obtaining such information is well-defined - that's why I added helper script for collecting such information from protein data bank files (in folder julia_scripts). I want to rewrite my own implementation in julia (to make that implementation publicly available), that's why this helper script will be written in that language.
