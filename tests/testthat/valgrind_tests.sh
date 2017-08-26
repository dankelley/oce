# How to run this test.
#
# 1. Type the following in the shell. It will take of order 10 to 30
# minutes to complete.
#
#       sh valgrind_tests.sh > valgrind_tests.out 2>&1
#
# 2. Look for string
#
#       Invalid read of size 8
#
# in the file valgrind_test.out. This string tells of a problem in reading past
# the end of an array. Note the name of the offending .c file, and the line
# number. (PS. ignore other output, which suggest problems that are 
# just from the R package itself ... it is possible to rebuild R to avoid
# these problems, but that is a *hard* task, and keeping track of 
# two versions of R is tricky.)
#
# 3. Alternatively, find all the "Invalid read" strings by typing
# the following in the shell.
#
#    grep -5 "Invalid read" valgrind_tests.out
#

cd .. ; echo 'library(oce); library(ocedata); library(testthat); test_check("oce")' | R --vanilla -d "valgrind --tool=memcheck --leak-check=full --track-origins=yes -v"

