# make sure ContestApplet.jar is in your CLASSPATH
# JAVA_OPT=-classpath .:./classes/ContestApplet.jar
JAVA_OPT=-classpath .:f:/progenv/topcoder/ContestApplet.jar

VERSION=0.2

all: TCEmacs.jar

TCEmacs.jar: TCEmacs.class GNUClient.class GNUClientException.class
	jar cvf $@ $^

%.class: %.java
	javac $(JAVA_OPT) $<

TCEmacs.class: GNUClient.class

dist: TCEmacs.jar
	rm -rf tc_emacs
	mkdir tc_emacs-$(VERSION)
	cp -a TCEmacs.jar TCEmacs.java GNUClient.java topcoder.el README \
		COPYING ChangeLog Makefile tc_emacs-$(VERSION)
	tar czf tc_emacs-$(VERSION).tar.gz tc_emacs-$(VERSION)
	rm -rf tc_emacs-$(VERSION)

clean:
	rm -f *~
