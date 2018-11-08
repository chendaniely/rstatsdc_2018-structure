all:
	Rscript -e "if(!require(webshot)){install.packages('webshot'); webshot::install_phantomjs()}"
	Rscript -e 'file_name <- paste0("file://", normalizePath("slides.html")); webshot::webshot(file_name, "slides.pdf")'
