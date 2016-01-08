# sdr_tools

This repo originally was supposed to contain code for finding specificity determining regions (with draft code in Scala).

Here are files & code in Scala I (@latticetower) promised to write.

I wrote folding implementation, qhull-based tesselation code, and now I decided to officially give up.

There is still much to do - to move from folding to alascan, to refactor code, et cetera, et cetera, et cetera, but...

Currently is repo is not actively maintained, because I decided there is no point to continue writing something to person who don't even respect me as a software developer and who don't treat me as a friend (yeah, we Russians are like bears: we prefer to work for free for someone who respects us, or who pretends to be our friend; promises given to those people are sacred). So-called zombie in code isn't a real reason to stop. There is only one reason for giving up in something - it's people.

I wrote that code to some point and transferred it to person to whom I promised to write it. @zmactep, I hope you'll feel ashamed. If not, than open my ugly code and feel ashamed because I transferred it to you, and it is kind-a yours now.

Because of you I'll always remember that in some moment everything my so-called friends/collegues tell me - even that they like my code - could turn up to be just a cruel joke. That's why I'll never have ones. You ruined my career, my life. I just want to die.

I'll forgive you if you apologize.

License
===================
Future or current @zmactep's collegues can do whatever they want (but since than they are considered to be evil bastards), everyone else should ask them. I still reserve the right to commit sometimes, not because I promised it, but because being unemployed housewife makes me sad, writing code makes me feel better. I'm considered to be cursed to, because I'm a useless housewife (and remain to be unemployed - because in Russia 30-years old women without kids in practice can't get any job, especially if they don't want to try, because even their best friends betrayed them and lied to them).

Remarks
===================

Some of todos can be found in `scala_code/README.md` (in russian). There is also option to add specificity determining residues guessing algorithm to old code in python (branch `python_proto`).

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
