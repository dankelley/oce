# How to run this test.
#
# 1. run as below, in the shell
#       sh valgrind_tests.sh > valgrind_tests.out 2>&1
#
# 2. Look for string
#       Invalid read of size 8
# in the file valgrind_test.out; this signals a problem in reading past
# the end of an array. We do not get line numbers from this, but we find
# out the name of the .c file, which is a start (and all we need for the
# error in the submitted 0.9.20). NOTE: I think building 'oce' with
# valgrind built-in would tell us the line number, and if I figure that out,
# I will upate this comment to say how to do that.
#
# 3. Find all such strings
#    grep -5 "Invalid read" valgrind_tests.out
#
echo "library(testthat);source('test_accessors.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_adp.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_allclass.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_argo.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_astronomical.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_coastline.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_ctd.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_curl.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_datasets.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_flags.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_geod.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_landsat.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_adp.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_adv.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_argo.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_bremen.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_cm.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_coastline.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_ctd.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_echosounder.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_gps.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_index.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_lobo.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_satellite.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_local_section.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_map.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_misc.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_oce.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_plotting.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_rsk.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_section.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_sw.R')" | R --vanilla -d valgrind
echo "library(testthat);source('test_units.R')" | R --vanilla -d valgrind
