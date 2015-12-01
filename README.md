# sdr_tools
code for finding specificity determining regions (with draft code in Scala)

I planned to use this repo for my thesis scripts, and decided to open MS_thesis instead, and to use this repo for saving files & code in Scala I promised to write.
I'll put final code version after refactoring to zmactep/ig-toolkit, this repo will contain some additional benchmarks and test scripts.

references (mc, additional):

http://www.actabp.pl/pdf/2_2004/349s.pdf

Robert L. Jernigan. Protein folds. Curr.Opinion in Struct.Biology, 1992, 2:248-256


"In the final stage of the folding protocol, the entire full atom structures may be reconstructed"

Skolnick, J., Kolinski, A., Brooks, C.L., 111, Godzik, A. A method for prediction of protein structure from sequence. Current Biol. 3:414-423, 1993. -

= details on full-atom model reconstruction

Michael Feig, Piotr Rotkiewicz, Andrzej Kolinski, Jeffrey Skolnick and Charles L. Brooks III.
Accurate Reconstruction of All-Atom Protein Representations From Side-Chain-Based Low-Resolution Models

= julia_scripts

It turned out that there is no information on pdb backbone vectors statistics, at least I couldn't find any. Method of obtaining such information is well-defined - that's why I added helper script for collecting such information from protein data bank files (in folder julia_scripts). I want to rewrite my own implementation in julia (to make that implementation publicly available), that's why this helper script will be written in that language.

TODO
----

- [ ] переписать энергию водородных связей: вместо имеющегося критерия использовать критерий из статьи 163.pdf
- [ ] добавить описание 163.pdf в презентацию
- [x] считать среднее значение для основной цепи другим способом: мне нужен не просто средний вектор, а вектор, длина которого средняя из всех длин других векторов, углы - тоже что-то посередине
