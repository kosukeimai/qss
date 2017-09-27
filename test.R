#!/usr/bin/env Rscript

for (fn in list.files(".", recursive=TRUE, pattern=".Rmd")) {
    rmarkdown::render(fn, "pdf_document") 
    unlink(gsub("\\.Rmd$", ".pdf", fn))
}

