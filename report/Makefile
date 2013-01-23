.PHONY:
hex_board_3x3: images/hex_board_3x3.svg
	inkscape -D -z --file=images/hex_board_3x3.svg --export-pdf=hex_board_3x3.pdf --export-latex 

.PHONY:
hex_board_3x3_positional_game:
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$@$$image ; \
	done ; 

thesis: thesis.tex hex_board_3x3
	pdflatex thesis.tex

clean: 
	rm -rf thesis.pdf thesis.aux thesis.log hex_board_3x3.pdf_tex hex_board_3x3.pdf *.log hex_board_3x3_positional_game*.pdf hex_board_3x3_positional_game*.pdf_tex
