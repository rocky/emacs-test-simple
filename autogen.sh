#!/bin/sh
cp README.md README
autoreconf -vi && \
autoconf && {
  echo "Running configure with --enable-maintainer-mode $@"
  ./configure --enable-maintainer-mode $@
}
