CURDIR      := $(TOPDIR)/hsrc/Syntax

SYNFILES := Lexer ParserCore AST Parser

SYNFILES_PREFIXED := $(addprefix $(CURDIR)/,$(SYNFILES))
OBJECTS        += $(addsuffix .o,$(SYNFILES_PREFIXED))
HS_SOURCES     += $(addsuffix .hs,$(SYNFILES_PREFIXED))

EXTRA_CLEAN += $(CURDIR)/Lexer.hs  $(CURDIR)/Lexer.info  \
			   $(CURDIR)/Parser.hs $(CURDIR)/Parser.info

.SECONDARY: $(CURDIR)/Lexer.hs $(CURDIR)/Parser.hs

$(CURDIR)/Parser.d: $(CURDIR)/Lexer.d
