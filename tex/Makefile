.PHONY: main.pdf all clean
MAINFILE=main

all: $(MAINFILE).pdf

$(MAINFILE).pdf: $(MAINFILE).tex
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode -synctex=1 -shell-escape" -use-make -bibtex $(MAINFILE)

clean:
	latexmk -CA
	rm -rf _minted-$(MAINFILE)
	rm -f $(MAINFILE).bbl $(MAINFILE).pyg $(MAINFILE)-gnuplot* 
