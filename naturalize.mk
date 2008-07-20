prefix = /usr/local
libdir = $(prefix)/lib/naturalize
bindir = $(prefix)/bin
modules = naturalize.scm interface.scm options.scm

install:
	mkdir $(libdir)
	cp $(modules) $(libdir)

	cp dist.scm $(bindir)/naturalize
	chown root $(bindir)/naturalize
	chgrp root $(bindir)/naturalize
	chmod +x $(bindir)/naturalize
