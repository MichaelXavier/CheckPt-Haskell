CC = runhaskell Setup.hs
CONFIG_FLAGS = --user
TEST_FLAGS = -ftest

all: build

clean: Setup.hs
	$(CC) clean

build: config
	$(CC) build

install: build
	$(CC) install

build_test: config_test
	$(CC) build

config: CheckPt.cabal CheckPt.hs Setup.hs CheckPt
	$(CC) $(CONFIG_FLAGS) configure

config_test: CheckPt.cabal CheckPt.hs Setup.hs CheckPt test/
	$(CC) $(TEST_FLAGS) $(CONFIG_FLAGS) configure

test: build_test
	$(CC) test
