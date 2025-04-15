setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rmarkdown::render("Phân loại hoạt động con người từ dữ liệu cảm biến điện thoại bằng học máy.Rmd", output_format = "word_document")
rmarkdown::render("Phân loại hoạt động con người từ dữ liệu cảm biến điện thoại bằng học máy.Rmd", output_format = "pdf_document")
