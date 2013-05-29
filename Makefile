# Makefile
CHDIR_SHELL := $(SHELL)
VIRTUALENV=venv-py27
R=Rscript
objects=src/scrapper/download_scores.pyc cache clean-data/unam-admission.csv
define chdir
$(eval _D=$(firstword $(1) $(@D)))
$(info $(MAKE): cd $(_D)) $(eval SHELL = cd $(_D); $(CHDIR_SHELL))
endef


all: scrape

$(VIRTUALENV): $(VIRTUALENV)/bin/activate

$(VIRTUALENV)/bin/activate: ./src/scrapper/requirements.txt
	$(call chdir, src/scrapper)
	test -d $(VIRTUALENV) || virtualenv --python=python2.7 $(VIRTUALENV)
	. $(VIRTUALENV)/bin/activate; pip install -Ur requirements.txt
	touch $(VIRTUALENV)/bin/activate

download: $(VIRTUALENV)
	$(call chdir, src/scrapper)
	. $(VIRTUALENV)/bin/activate; python download_scores.py

scrape: download ./clean-data/unam-admission.csv

charts: ./src/create-charts.R
	$(call chdir, src)	
	$(R) create-charts.R 

.PHONY : clean
clean :
	-rm edit $(objects)
