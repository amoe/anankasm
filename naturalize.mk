prefix = /usr/local
libdir = $(prefix)/lib/naturalize
bindir = $(prefix)/bin
modules = taglib.scm \
          munge-tag.scm \
          naturalize.scm \
          interface.scm \
          options.scm \
          histogram.scm \
          replaygain.scm

install:
	mkdir -p $(libdir)
	cp $(modules) $(libdir)

	cp dist.scm $(bindir)/naturalize
	chown root $(bindir)/naturalize
	chgrp root $(bindir)/naturalize
	chmod +x $(bindir)/naturalize

uninstall:
	rm -r $(libdir)
	rm $(bindir)/naturalize
