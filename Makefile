HAS_ELIXIR=1
NO_REGISTRY_UPDATE=1
# NO_XREF=1

include bu.mk

distclean::
	$(verbose) $(RM_RF) doc
	$(verbose) $(RM_RF) .c_build priv/vice_facedetect.so

release: dist lint tag ## Tag and release to hex.pm
	$(verbose) $(REBAR) hex publish
