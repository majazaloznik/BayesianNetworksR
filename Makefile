# VARIABLE DEFINITIONS  #######################################################
###############################################################################
# FOLDERS #####################################################################
DIR := .#
CODE := $(DIR)/code#

DOCS := $(DIR)/docs#

DATA := $(DIR)/data

FIG := $(DIR)/figures

VPATH = $(CODE)
# FILES #######################################################################

chapters := $(addprefix $(DOCS)/, $(notdir $(subst .R,.pdf, $(wildcard $(CODE)/*.R))))\
$(addprefix $(DOCS)/, $(notdir $(subst .R,.html, $(wildcard $(CODE)/*.R))))

# COMMANDS ####################################################################


# recipe to knit pdf from R script
define render-pdf
@echo creating the $(@F) file by spinning the R script ------------------------
  Rscript -e "suppressWarnings(suppressMessages(require(rmarkdown)));\
render('$<',  output_format = 'pdf_document',\
quiet = TRUE)"
mv $(<D)/$(@F) $(DOCS)
-rm $(wildcard ./code/tex2pdf*) -fr
@echo done
endef 

# recipe to knit HTML from R script
define render-html
@echo creating the $(@F) file by spinning the R script ------------------------
  Rscript -e "suppressWarnings(suppressMessages(require(rmarkdown))); \
render('$<', output_format = 'html_document',\
quiet = TRUE )"
mv $(<D)/$(@F) $(DOCS)
@echo done
endef 

# FUNCTIONS ###################################################################

# render all R scripts to  pdf. works because of vpath looks in code folder
$(DOCS)/%.pdf: %.R
	@$(render-pdf)
	
# render all R scripts to  pdf. works because of vpath looks in code folder
$(DOCS)/%.html: %.R
	@$(render-html)
	
# DEPENDENCIES   ##############################################################
###############################################################################

.PHONY: all 

all: $(chapters)






