The files test schemes for subsetting coastline files.

# 01.R

Delete 'island' portions that have no intersection with a chosen polygon.

# 02.R

First test with cutting polygons on a vertical line (here, to the east of a
point clicked on the plot). This is a slow operation, but (a) a person will
only need to use it in constructing coastline subsets to speed up individual
projects, so a 1-minute operation is not a real pain and (b) it is purposely
written in an indexing/looping style, so it can be made into C easily.

# 03.R

Trim four sides (but wrong on step 3)

# 04.R

Still broken (in a new way)

# 05.R

Trial solution using raster package functions.

