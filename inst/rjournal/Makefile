all: hankin_frab.Rnw hankin_frab.bib figures/commuting_diagram.tex RJwrapper.tex motivation-letter/motivation-letter.md
	cd figures && pdflatex commuting_diagram
	cd motivation-letter && pandoc motivation-letter.md -o motivation-letter.pdf
	R CMD Sweave hankin_frab.Rnw
	R CMD Stangle hankin_frab.Rnw
	pdflatex RJwrapper
	bibtex   RJwrapper
	pdflatex RJwrapper
	pdflatex RJwrapper
	zip --must-match hankin_frab.zip hankin_frab.Rnw hankin_frab.tex hankin_frab.bib RJwrapper.tex RJwrapper.pdf figures/commuting_diagram.pdf hankin_frab.R motivation-letter/motivation-letter.pdf

clean:
	rm -f *.zip *.pdf *.Rout *.aux *.bbl *.log figures/*.pdf figures/*.aux figures/*.log motivation-letter/motivation-letter.pdf
