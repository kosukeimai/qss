#!/usr/bin/env Rscript

for (fn in list.files(".", recursive=TRUE, pattern=".Rmd")) {
    rmarkdown::render(fn, "html_document") 
    unlink(gsub("\\.Rmd$", ".html", fn))
}

