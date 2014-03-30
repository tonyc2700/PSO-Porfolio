# ---------------------------------------------------------------------------
#
#  Makefile for HonsReport.tex, sample Latex template for project reports
#
#  Dave Ritchie 28/10/02
#
#  On raven, you will probably need "setup tex" in your .login file
#
#  Usage: "make" to make the Postscript, 
#         "make bib" to update the bibliography (needs to re-run latex)
#
# ---------------------------------------------------------------------------

all:	pdf

ps:	HonsReport.ps

pdf:	HonsReport.pdf

bib:	HonsReport.bbl


HonsReport.dvi:	HonsReport.tex
	latex HonsReport.tex

HonsReport.bbl:	HonsReport.bib HonsReport.dvi
	latex HonsReport;  \
	bibtex HonsReport; \
	latex HonsReport;

HonsReport.ps:	HonsReport.dvi
	dvips -Pwww -o HonsReport.ps HonsReport.dvi

HonsReport.pdf:	HonsReport.ps HonsReport.bbl
	ps2pdf HonsReport.ps HonsReport.pdf


clean:	
	rm -f *.log *.aux *.dvi *.toc *.blg

vclean:	clean
	rm -f *.bbl *.ps *.pdf
