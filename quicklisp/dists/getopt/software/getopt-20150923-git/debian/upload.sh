#!/bin/bash -e

dup getopt -Ufiles.kpe.io -D/home/ftp/getopt  -C"(umask 022; /home/kevin/bin/remove-old-versions getopt latest)" -su $*



