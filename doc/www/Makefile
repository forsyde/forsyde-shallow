# The {ingo,axel}.bib phony rule is used to force the download (and update) of
# the files no matter the modification date of previosly downloaded
# versions
.PHONY: clean ingo.bib axel.bib

publications.html: axel.bib ingo.bib
	bib2bib -q -ob ForSyDe.bib -c '(key : "ForSyDe") and ($$type <> "COMMENT" )' axel.bib ingo.bib 
	bibtex2html -q -d -r -s alpha -noheader -nofooter  -css style.css -t 'ForSyDe Publications' -o publications ForSyDe.bib 

clean: 
	rm -f ForSyDe.bib ingo.bib axel.bib publications.html publications_bib.html

ingo.bib: 
	wget -q -O ingo.bib http://web.it.kth.se/~ingo/ForSyDe-ingo.bib 

axel.bib: 
	wget -q -O axel.bib http://web.it.kth.se/~axel/ForSyDe-axel.bib 
