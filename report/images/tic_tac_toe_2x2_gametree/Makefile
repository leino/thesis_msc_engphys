tic_tac_toe_2x2_gametree:	*.svg
	for svgfile in *.svg ; do \
		filename=$$(basename $$svgfile) ; \
		filename=$${filename%.*} ; \
		inkscape -D -z --file=$$svgfile --export-pdf=$@_$$filename.pdf --export-latex ; \
	done ; 

clean:
	rm -rf *.pdf *.pdf_tex