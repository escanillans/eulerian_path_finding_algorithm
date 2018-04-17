# eulerian_path_finding_algorithm

This program takes as input a set of k-mers and outputs a single superstring that has a k-mer spectrum equal to the set of input k-mers. The algorithm I used was the Eulerian path finding algorithm.

To read more on Eulerian path visit https://en.wikipedia.org/wiki/Eulerian_path.

To run the script there are two options and they are as follows:

Rscript euler_assemble.R test_cases/k_num.kmers.txt

or

sh euler_assemble.sh test_cases/k_num.kmers.txt

Note: k_num is something you must set depending on the files in test_cases in which you would like to test. k is the length of substring and num is the length of the desired superstring.
