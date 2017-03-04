# qss
[![Build Status](https://travis-ci.org/kosukeimai/qss.svg?branch=master)](https://travis-ci.org/kosukeimai/qss)
Supplementary Materials for the book,
**[Quantitative Social Science: An Introduction](http://press.princeton.edu/titles/11025.html)**,
published by Princeton University Press in 2017.  It is
also available for purchase at vendors like
[Amazon](https://www.amazon.com/Quantitative-Social-Science-Kosuke-Imai/dp/0691175462).

The book is based on the teaching philosophy summarized in the talk I
gave at the Nuffield Foundation's
[Q-Step Programme](http://www.nuffieldfoundation.org/q-step) in 2015: 
[slides](http://imai.princeton.edu/talk/files/Q-Step15.pdf) 

This repository contains the data sets and **R** scripts for all of the chapters:

1. [Introduction](INTRO)
2. [Causality](CAUSALITY)
3. [Measurement](MEASUREMENT)
4. [Prediction](PREDICTION)
5. [Discovery](DISCOVERY)
6. [Probability](PROBABILITY)
7. [Uncertainty](UNCERTAINTY)

In addition, the repository contains:

1. [Sample course syllabi](syllabus)

## R package `qss`

The data and code in this repository are also available as an
[R package `qss`](https://github.com/kosukeimai/qss-package) 
(see [the package website](https://kosukeimai.github.io/qss-package/)). The code is in 
the form of vignettes. To install this package, use the following command:

    install.packages("devtools") # if you have not installed devtools package already
    devtools::install_github("kosukeimai/qss-package", build_vignettes = TRUE)
    
Once the `qss` package is installed, you can use the data and vignette:

    library(qss)
    data(package = "qss") # list all data sets
    data(elections) # load the elections data
    vignette(package = "qss") # list all vignettes
    browseVignette("qss") # list vignettes and R code
    vignette("causality", package = "qss") # show the vignette for the Causality chapter

## Related repositories

1. [swirl exercises `qss-swirl`](https://github.com/kosukeimai/qss-swirl)
2. [R package `qss`](https://github.com/kosukeimai/qss-package) ([the package website](https://kosukeimai.github.io/qss-package/))
3. [tidyverse code `qss-tidy`](https://github.com/jrnold/qss-tidy) 
4. [instructors' materials `qss-inst`](https://github.com/kosukeimai/qss-inst)
   (private repository)

