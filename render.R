setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rmarkdown::render("report.Rmd", output_format = "word_document")
rmarkdown::render("report.Rmd", output_format = "pdf_document")
