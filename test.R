#!/usr/bin/env Rscript

for (fn in list.files(".", recursive=TRUE, pattern=".Rmd")) {
    knitr::purl(fn, output = paste0(fn,".R"), documentation = 2); 
    setwd(dirname(fn)); 
    getwd(); 
    source(paste0(basename(fn),".R")); 
    unlink(paste0(basename(fn),".R")); 
    setwd("../")
}

