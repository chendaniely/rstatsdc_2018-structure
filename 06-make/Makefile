BILLBOARD=./analysis/billboard_eda/

all : commands

## commands      : show all commands.
commands :
	@grep -E '^##' Makefile | sed -e 's/## //g'

## billboard_eda  : re-generate billboard eda analsyis
billboard_eda :
	Rscript ${BILLBOARD}/01*
	Rscript ${BILLBOARD}/02-01*
	Rscript ${BILLBOARD}/02-02*
	Rscript ${BILLBOARD}/02-03*
	Rscript -e "rmarkdown::render(here::here('./analysis/billboard_eda/03-eda.Rmd'), output_dir = './output/billboard_reports')"
	Rscript -e "rmarkdown::render(here::here('./analysis/billboard_eda/04-model.Rmd'), output_dir = './output/billboard_reports')"

## clean         : clean up junk files.
clean :
	find data/processed/ -type f -name '*.csv' | xargs rm
	find analysis/ type f -name '*.html' | xargs rm
