library(tools)

texi2pdf(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/Exercise3.tex"))
