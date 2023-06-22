load(file="~/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/r/m05_vector_beam.rda")
oldBeam <- m05VectorBeam
oldV <- oldBeam@data$v[1:10, ]

load("m05_vector_beam.rda") # created by local m05_vector.R (not old-format file CR sent 2023-06-20)
newBeam <- m05VectorBeam
newV <- newBeam@data$v[1:10, ]
stopifnot(identical(oldV, newV))
cat("beam velocities match ok\n")

# Compare items in metadata.  Note that the new version gains
# a 'flags' item, and loses the 'numberOfCells' item (which is
# meaningless for an adv) and the 'tiltSensorOrientation' item
# (which is not used anywhere in code).
oldMnames <- names(oldBeam@metadata)
newMnames <- names(newBeam@metadata)
stopifnot(identical(sort(c("flags", oldMnames)), sort(c("tiltSensorOrientation", "numberOfCells", newMnames))))
cat("beam metadata ok (new gains 'flags' and loses 'numberOfCells' and 'tiltSensorOrientation'\n")

#cat("old metadata elements: ", paste(oldMnames, collapse=" "), "\n")
#cat("new metadata elements: ", paste(newMnames, collapse=" "), "\n")

