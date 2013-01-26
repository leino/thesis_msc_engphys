all:	*.svg
	for svgfile in *.svg ; do \
		filename=$$(basename $$svgfile) ; \
		filename=$${filename%.*} ; \
		inkscape -D -z --file=$$svgfile --export-pdf=$$filename.pdf --export-latex ; \
	done ; 

clean:
	rm -rf *.pdf *.pdf_tex