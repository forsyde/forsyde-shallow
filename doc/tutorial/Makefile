
FNAME=tutorial
DOCBOOK_XSL_DIR=/opt/local/share/xsl/docbook-xsl
COMMON_XSL_OPS=section.autolabel 1

.PHONY: validate html txt html-chunks

validate: 
	msv ../docbook.rng $(FNAME).xml
#xmllint --valid --noout --nonet --relaxng ../docbook.rng  $(FNAME).xml

$(FNAME).html html: $(FNAME).xml
	xsltproc --stringparam $(COMMON_XSL_OPS) --stringparam html.stylesheet fptools.css --output $(FNAME).html $(DOCBOOK_XSL_DIR)/html/docbook.xsl $(FNAME).xml

html-chunks: $(FNAME).xml
	xsltproc --stringparam $(COMMON_XSL_OPS) --stringparam html.stylesheet fptools.css $(DOCBOOK_XSL_DIR)/html/chunk.xsl $(FNAME).xml

$(FNAME).pdf pdf:  $(FNAME).xml
	fop -param $(COMMON_XSL_OPS) -xml $(FNAME).xml -xsl $(DOCBOOK_XSL_DIR)/fo/docbook.xsl -pdf $(FNAME).pdf


$(FNAME).txt txt:  $(FNAME).xml
	fop -param $(COMMON_XSL_OPS) -xml $(FNAME).xml -xsl $(DOCBOOK_XSL_DIR)/fo/docbook.xsl -txt $(FNAME).txt