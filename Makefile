all: renv supplement ms

renv:
	Rscript -e "renv::restore()"

ms: ms.Rmd supplement.qmd
	Rscript -e 'rmarkdown::render("$<", "all")'

supplement: supplement.qmd
	quarto render supplement.qmd

clean:
	rm -rf supplement.pdf cache/ data/ *_files/ *_cache/ ms.pdf ms.docx ms.log

.PHONY: renv all clean