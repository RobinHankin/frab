all: hankin_frab.Rnw hankin_frab.tex hankin_frab.bib  RJwrapper.pdf motivation-letter/motivation-letter.pdf
	zip --must-match hankin_frab.zip hankin_frab.Rnw hankin_frab.tex hankin_frab.bib RJwrapper.tex RJwrapper.pdf figures/commuting_diagram.pdf hankin_frab.R motivation-letter/motivation-letter.pdf

hankin_frab.R: hankin_frab.Rnw
	R CMD Stangle hankin_frab.Rnw

hankin_frab.tex: hankin_frab.Rnw
	R CMD Sweave hankin_frab.Rnw

RJwrapper.pdf: hankin_frab.tex hankin_frab.bib RJwrapper.tex
	pdflatex RJwrapper
	bibtex   RJwrapper
	pdflatex RJwrapper
	pdflatex RJwrapper

motivation-letter/motivation-letter.pdf: motivation-letter/motivation-letter.md
	cd motivation-letter && pandoc motivation-letter.md -o motivation-letter.pdf

clean:
	rm -f *.zip *.pdf *.Rout *.aux *.bbl *.log motivation-letter/motivation-letter.pdf hankin_frab.tex
