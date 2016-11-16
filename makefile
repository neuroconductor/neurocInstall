neurocLite.R: neuro_top.R R/neuro_install.R make_neurocLite.R
	Rscript -e "source('make_neurocLite.R')"

clean: 
	rm neurocLite.R