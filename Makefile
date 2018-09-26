SELENIUM_PATH = "/usr/share/selenium-server/selenium-server-standalone.jar"

.PHONY: \
	i t r s install test repl selenium

i: install
t: test
r: repl
s: selenium

install:
	stack install

test:
	stack test

repl:
	stack repl

selenium:
	java -jar $(SELENIUM_PATH)
