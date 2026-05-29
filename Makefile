# vi:filetype=make:syntax=make:
# vi:noautoindent:expandtab:
.RECIPEPREFIX := $() $()
APP_SYSTEM = regex-visualizer
BINARY_NAME = regex-visualizer

DEP_URL = https://github.com/ediw8311xht/maximilian-utils.git
DEP_DIR = libs/maximilian-utils

.PHONY: all deps build clean-all clean clean-deps

all: deps build

clean-all: clean clean-deps

deps:
 @mkdir -p libs
 @if [ ! -d "$(DEP_DIR)" ] ; then \
  echo "Make: Cloning dependency..."; \
  git clone $(DEP_URL) $(DEP_DIR); \
 else \
  echo "Make: Updating dependency..."; \
  cd "$(DEP_DIR)" && git pull; \
  cd -; \
 fi

build:
 @echo "Building binary..."
 sbcl \
  --no-userinit \
  --no-sysinit \
  --load ~/quicklisp/setup.lisp \
  --eval '(require :asdf)' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(push (truename "$(DEP_DIR)/") asdf:*central-registry*)' \
  --eval '(ql:quickload :$(APP_SYSTEM))' \
  --eval '(asdf:make :$(APP_SYSTEM) :force t)'

clean:
 rm $(BINARY_NAME)

clean-deps:
 rm -r $(DEP_DIR)

