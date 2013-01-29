thesis: thesis.tex 
	pdflatex thesis.tex

.PHONY:
images: hex_board_3x3_positional_game hex_board_3x3_reverse_game hex_board_3x3_weak_game hex_board_3x3_reverse_weak_game tic_tac_toe_2x2_gametree

.PHONY:
hex_board_3x3_positional_game: images/hex_board_3x3_positional_game/*.svg
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$$image ; \
	done ;

.PHONY:
hex_board_3x3_reverse_game: images/hex_board_3x3_reverse_game/*.svg
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$$image ; \
	done ;

.PHONY:
hex_board_3x3_weak_game: images/hex_board_3x3_weak_game/*.svg
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$$image ; \
	done ;

.PHONY:
hex_board_3x3_reverse_weak_game: images/hex_board_3x3_reverse_weak_game/*.svg
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$$image ; \
	done ;

.PHONY:
tic_tac_toe_2x2_gametree: images/tic_tac_toe_2x2_gametree/*.svg
	cd images/$@; make; \
	for image in *.pdf_tex *.pdf ; do \
		mv $$image ../../$$image ; \
	done ;

clean: 
	rm -rf *.pdf *.pdf_tex *.log thesis.aux
