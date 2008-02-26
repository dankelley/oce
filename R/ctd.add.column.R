ctd.add.column <- function (x, column=NULL, column.name="",
                            code="", name="", unit="",
                            debug = FALSE)
{
    if (length(column) < 1) stop("must supply column data")
    if (column.name == "")  stop("must supply 'column.name'")
    if (code=="")           stop("must supply 'code'")
    if (name=="")           stop("must supply 'name'")
    if (unit=="")           stop("must supply 'unit'")
    result <- x
    insert.in.header <- function(h, flag, content, content.name)
    {
        last.was.flag <- FALSE
        after <- -1
        flags <- 0 # how many flagged lines exist on input
        pattern <- paste("^#[\t ]*", flag)
        n <- length(h)
        for (i in 1:n) {
            if (flag == "name") { # increment nquan (skip on e.g. "span")
                g <- grep("#[\t ]*nquan[\t ]*=[\t ]*", h[i], perl=TRUE, useBytes=TRUE)
                if (length(g)) {
                    nquan <- unlist(strsplit(h[i], "\\s"))[4]
                    nquan.new <- as.character(1 + as.integer(nquan))
                    h[i] <- sub(nquan, nquan.new, h[i])
                }
            }
            if (last.was.flag) {
                if (!length(grep(pattern, h[i], perl=TRUE, useBytes=TRUE))) {
                    after <- i
                    break
                }
            }
            cat("grep(\"", pattern, "\", \"", h[i], "\",...) -> ", length(grep(pattern,h[i],perl=TRUE,useBytes=TRUE)), "\n",sep="")
            if (length(grep(pattern, h[i], perl=TRUE, useBytes=TRUE))) {
                last.was.flag <- TRUE
                flags <- flags + 1
            }
        }
        if (after < 1)
            stop("Cannot locate any flagged lines in input header")
        if (debug)
            cat("after=", after, "\n", "\t", h[after-1], "\n\t", h[after])
        return(c(h[1:(after-1)],
                 paste("# ", flag, " ", flags, " = ", content, sep=""),
                 h[after:n]))
    }
    h <- result$metadata$header
    h <- insert.in.header(h, "name", sprintf("%s: %s, [%s]", code, name, unit))
    r <- range(column)
    h <- insert.in.header(h, "span", sprintf("%f, %f",r[1],r[2]))
    if (debug) {
        cat("Original header:", result$header,sep="\n")
        cat("Modified header:", h,sep="\n")
    }
    result$metadata$header <- h
    result$data[,column.name] <- column
    log.item <- paste("modified by ctd.add.column(x, column, column.name=\"",
                      column.name,
                      "\", code=\"", code,
                      "\", name=\"", name,
                      "\", unit=\"", unit,
                      "\", debug)",sep="")
    result <- processing.log.append(result, log.item)
    return(result)
}
