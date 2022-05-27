hash <- "whoHXig870kv" # 01.md
hash <- "ce9l531L58hj" # 02.md
hash <- "DuSX46BESmx8" # 03.md

options(width=200)

url <- paste0("https://win-builder.r-project.org/", hash, "/examples_and_tests/oce-Ex.timings")
d <- read.table(url, header=TRUE)
dd <- d[order(d$user, decreasing=TRUE),]
total <- sum(dd$user)
dd$percent <- round(100*dd$user / total,1)
#dd$cumpercent <- round(cumsum(100*dd$user / total),1)
# trim things we don't care about
dd$system <- NULL
dd$elapsed <- NULL
cat("Total time: ", total, "s\n")
cat("Next are items taking more than 0.1 s\n")
print(dd[dd$user >= 0.1,])
