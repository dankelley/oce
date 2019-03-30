if (requireNamespace("RCurl", quietly=TRUE)) {
    debug <- FALSE
    files <- list.files(pattern="*.R$")
    files <- files[-which(files == "check_urls.R")]
    for (file in files) {
        l <- readLines(file)
        u <- grep("\\\\url", l)
        if (length(u)) {
            message(file)
            l <- l[u]
            for (li in l) {
                li2 <- gsub("^.*\\\\url\\{([^\\{]*)\\}.*$", "\\1", li)
                if (nchar(li2) < 1)
                    message(" odd line: '", li, "' yielding zero-length url component")
                if (debug) {
                    message(" >  '", li, "'")
                    message("  > '", li2, "'")
                }
                ok <- RCurl::url.exists(li2)
                if (!ok) {
                    message(" >  '", li, "'")
                    message("  > '", li2, "'")
                    message(" ERROR")
                }
            }
        }
    }
}
