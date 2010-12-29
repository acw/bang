CURDIR := $(TOPDIR)/hsrc

HSRC_FILES := Main

HSRC_FILES_PREFIXED := $(addprefix $(CURDIR)/,$(HSRC_FILES))
OBJECTS        += $(addsuffix .o,$(HSRC_FILES_PREFIXED))
HS_SOURCES     += $(addsuffix .hs,$(HSRC_FILES_PREFIXED))

include $(CURDIR)/Syntax/Makefile
