# merely test whether we can read files
library(oce)
for (file in list.files(path="developer_only", pattern=".*")) {
    if (file != "README.md") {
        message("file: ", file)
        d <- read.oce(paste("developer_only", file, sep="/"))
        summary(d)
    }
}
