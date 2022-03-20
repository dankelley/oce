hash <- "whoHXig870kv"

options(width=200)
url <- paste0("https://win-builder.r-project.org/", hash, "/examples_and_tests/oce-Ex.timings")
d <- read.table(url, header=TRUE)
dd <- d[order(d$user, decreasing=TRUE),]
total <- sum(dd$user)
print(total)
dd$percent <- round(100*dd$user / total,1)
dd$cumpercent <- round(cumsum(100*dd$user / total),1)
dd[dd$user > 0.5,]
