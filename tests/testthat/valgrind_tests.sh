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
echo "source('test_accessors.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_adp.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_allclass.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_argo.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_astronomical.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_coastline.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_ctd.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_curl.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_datasets.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_flags.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_geod.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_landsat.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_adp.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_adv.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_argo.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_bremen.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_cm.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_coastline.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_ctd.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_echosounder.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_gps.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_index.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_lobo.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_satellite.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_local_section.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_map.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_misc.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_oce.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_plotting.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_rsk.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_section.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_sw.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
echo "source('test_units.R')" | R --vanilla -d "valgrind --tool=memcheck --leak-check=full"
