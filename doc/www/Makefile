
.PHONY= clean


publications.html: axel.bib
	bib2bib -q -ob ForSyDe.bib -c 'key : "ForSyDe"' axel.bib 
	bibtex2html -q -d -r -s alpha -noheader -nofooter  -css style.css -t 'ForSyDe Publications' -o publications ForSyDe.bib 

clean: 
	rm -f ForSyDe.bib ingo.bib axel.bib publications.html publications_bib.html

ingo.bib:
	wget 

axel.bib:
	wget -q -O axel.bib http://web.it.kth.se/~axel/ForSyDe-axel.bib 
