GHC          ?= ghc
GHC_FLAGS    ?= -Wall -i$(TOPDIR)/hsrc -XMultiParamTypeClasses
ALEX         ?= alex
ALEX_FLAGS   ?=
HAPPY        ?= happy
HAPPY_FLAGS  ?=
SED          ?= sed -E
FIND         ?= find
RM           ?= rm
PACKAGES      = monadLib bytestring utf8-string

TARGET       ?= bang
TOPDIR       := $(shell pwd)
CURDIR       := $(TOPDIR)
GHC_PACKAGES := $(addprefix -package ,$(PACKAGES))
OBJECTS      :=

.PHONY: all
all: $(TARGET)

include $(CURDIR)/mk/build.mk
include $(CURDIR)/hsrc/Makefile

LIBS    := monadLib
DEPENDS := $(HS_SOURCES:.hs=.d)

$(TARGET): $(OBJECTS)
	$(call cmd,ghc_ld) $(GHC_PACKAGES)

.PHONY: clean
clean:
	$(FIND) . -name '*.d' -delete
	$(FIND) . -name '*.hi' -delete
	$(FIND) . -name '*.o' -delete
	$(RM) -f $(TARGET) $(OBJECTS) $(EXTRA_CLEAN)

foo:
	@echo $(DEPENDS)

-include $(DEPENDS)
