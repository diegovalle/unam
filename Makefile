# Makefile
CHDIR_SHELL := $(SHELL)
VIRTUALENV=venv_py27
GRAPHDIR = graphs/
vpath %.Rout ./src
objects=src/scrapper/download_scores.pyc cache/index cache/pages clean-data/unam-admission.csv
REQUIREMENTS=./src/scrapper/requirements.txt
define chdir
$(eval _D=$(firstword $(1) $(@D)))
$(info $(MAKE): cd $(_D)) $(eval SHELL = cd $(_D); $(CHDIR_SHELL))
endef

.PHONY: all virtualenv scrape charts clean png
all: virtualenv scrape charts png


virtualenv: src/scrapper/$(VIRTUALENV)/bin/activate

src/scrapper/$(VIRTUALENV)/bin/activate: $(REQUIREMENTS)
	$(call chdir, src/scrapper)
	test -d $(VIRTUALENV) || virtualenv --python=python2.7 $(VIRTUALENV)
	. $(VIRTUALENV)/bin/activate; pip install -r requirements.txt
	touch $(VIRTUALENV)/bin/activate

./clean-data/unam-admission.csv: ./src/scrapper/download_scores.py
	$(call chdir, src/scrapper)		
	. $(VIRTUALENV)/bin/activate; python download_scores.py

scrape: ./clean-data/unam-admission.csv

charts: create-charts.Rout

create-charts.Rout: src/create-charts.R
	$(call chdir, src)	
	R CMD BATCH --no-save create-charts.R

png: graphs/png/mecatronica.svg.png

graphs/png/mecatronica.svg.png: graphs/mecatronica.svg
	$(call chdir, src)
	./svgtopng.sh


clean :
	-rm -f $(objects)
