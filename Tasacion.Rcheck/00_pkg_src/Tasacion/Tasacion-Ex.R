pkgname <- "Tasacion"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('Tasacion')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Tasacion-package")
### * Tasacion-package

flush(stderr()); flush(stdout())

### Name: Tasacion-package
### Title: Generar una tasacion
### Aliases: Tasacion-package Tasacion
### Keywords: package

### ** Examples
Tasacion("c:/tmp")



cleanEx()
nameEx("Tasacion")
### * Tasacion

flush(stderr()); flush(stdout())

### Name: Tasacion
### Title: Generar una tasacion
### Aliases: Tasacion
### Keywords: ~kwd1 ~kwd2

### ** Examples
Tasacion("c:/tmp")


### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
