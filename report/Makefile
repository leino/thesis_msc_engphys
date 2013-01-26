.PHONY:
hex_board_3x3: images/hex_board_3x3.svg
	inkscape -D -z --file=images/hex_board_3x3.svg --export-pdf=hex_board_3x3.pdf --export-latex 

.PHONY:
hex_board_3x3_positional_game: images/hex_board_3x3_positional_game/*.svg
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$@$$image ; \
	done ;

.PHONY:
hex_board_3x3_reverse_game: images/hex_board_3x3_reverse_game/*.svg
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$@$$image ; \
	done ;

.PHONY:
hex_board_3x3_weak_game: images/hex_board_3x3_weak_game/*.svg
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$@$$image ; \
	done ;

.PHONY:
hex_board_3x3_reverse_weak_game: images/hex_board_3x3_reverse_weak_game/*.svg
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$@$$image ; \
	done ;

.PHONY:
images: hex_board_3x3_positional_game hex_board_3x3_reverse_game hex_board_3x3_weak_game hex_board_3x3_reverse_weak_game

thesis: thesis.tex hex_board_3x3
	pdflatex thesis.tex

clean: 
	rm -rf *.pdf *.pdf_tex *.log thesis.aux
