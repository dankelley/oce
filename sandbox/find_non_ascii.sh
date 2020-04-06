Rscript -e "setwd('~/git/oce/man');\n
files <- list.files(pattern='*.Rd');\n
for (file in files) {\n
    cat('FILE \"', file, '\"...\n', sep='');\n
    tools:::showNonASCIIfile(file)\n
}"

