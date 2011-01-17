ifeq ($(V),)
  quiet		= quiet_
  Q			= @
else
  quiet		=
  Q			=
endif

echo-cmd = $(if $($(quiet)cmd_$(1)), echo "  $($(quiet)cmd_$(1))";)
cmd      = @$(echo-cmd) $(cmd_$(1))

# ghc
cmd_ghc_o_hs         = $(GHC) $(GHC_FLAGS) -c $<
quiet_cmd_ghc_o_hs   = GHC     $(notdir $@)
%.o : %.hs
	$(call cmd,ghc_o_hs)
%.hi : %.o
	@:

# ghc-depends
cmd_ghc_d_hs         = $(GHC) $(GHC_FLAGS) -M -dep-makefile $@ $<
quiet_cmd_ghc_d_hs   = DEPEND  $(notdir $@)
%.d : %.hs
	$(call cmd,ghc_d_hs)
	@$(SED) -i"" -e "s|: hsrc|: $(TOPDIR)/hsrc|g" $@

# ghc-ld
cmd_ghc_ld           = $(GHC) -o $@ $^
quiet_cmd_ghc_ld     = LD      $(notdir $@)

# alex
cmd_alex_hs_x        = $(ALEX) $(ALEXFLAGS) -i -o $@ $<
quiet_cmd_alex_hs_x  = ALEX    $(notdir $@)
%.hs : %.x
	$(call cmd,alex_hs_x)

# happy
cmd_happy_hs_y       = $(HAPPY) $(HAPPYFLAGS) -i -o $@ $<
quiet_cmd_happy_hs_y = HAPPY   $(notdir $@)
%.hs : %.y
	$(call cmd,happy_hs_y)

