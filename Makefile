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


# COMMANDS ####################################################################


# recipe to knit pdf from R script
define render-pdf
@echo creating the $(@F) file by knitting them in R. ------------------------
  Rscript -e "suppressWarnings(suppressMessages(require(rmarkdown)));\
render('$<', output_dir = '$(@D)', output_format = 'pdf_document',\
quiet = TRUE )"
-rm $(wildcard ./code/tex2pdf*) -fr
endef 

# recipe to knit HTML from R script
define render-html
@echo creating the $(@F) file by knitting them  in R.------------------------
  Rscript -e "suppressWarnings(suppressMessages(require(rmarkdown))); \
render('$<', output_dir = '$(@D)', output_format = 'html_document',\
quiet = TRUE )"
endef 

# FUNCTIONS ###################################################################


# top level dependencies #######################################################

chapters := $(addprefix $(DOCS)/, $(notdir $(subst .R,.pdf, $(wildcard $(CODE)/*.R))))

# journal (with graph) render to  pdf
$(DOCS)/%.pdf: %.R
	@$(render-pdf)
# DEPENDENCIES   ##############################################################
###############################################################################

.PHONY: all 

all: $(chapters)

test: 
		@echo $(chapters)




