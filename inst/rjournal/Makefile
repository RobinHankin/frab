all: hankin_frab.Rnw hankin_frab.bib figures/commuting_diagram.tex RJwrapper.tex
	cd figures && pdflatex commuting_diagram
	R CMD Sweave hankin_frab.Rnw
	R CMD Stangle hankin_frab.Rnw
	pdflatex RJwrapper
	bibtex   RJwrapper
	pdflatex RJwrapper
	pdflatex RJwrapper
	zip --must-match hankin_frab.zip hankin_frab.Rnw hankin_frab.tex hankin_frab.bib RJwrapper.tex RJwrapper.pdf figures/commuting_diagram.pdf hankin_frab.R

clean:
	rm *.pdf *.Rout *.aux *.bbl *.log
