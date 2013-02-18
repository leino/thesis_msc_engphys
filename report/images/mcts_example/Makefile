mcts_example_before:	*.svg
	for svgfile in *.svg ; do \
		filename=$$(basename $$svgfile) ; \
		filename=$${filename%.*} ; \
		inkscape -D -z --file=$$svgfile --export-pdf=$$filename.pdf --export-latex ; \
	done ;
mcts_example_after:	*.svg
	for svgfile in *.svg ; do \
		filename=$$(basename $$svgfile) ; \
		filename=$${filename%.*} ; \
		inkscape -D -z --file=$$svgfile --export-pdf=$$filename.pdf --export-latex ; \
	done ; 

clean:
	rm -rf *.pdf *.pdf_tex