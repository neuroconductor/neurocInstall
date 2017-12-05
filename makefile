neurocLite.R: neuro_top.R \
	R/neuro_install.R \
	R/neuro_package_table.R \
	R/current_neuroc_release.R \
	DESCRIPTION \
	make_neurocLite.R 
	Rscript -e "source('make_neurocLite.R')"

clean: 
	rm neurocLite.R