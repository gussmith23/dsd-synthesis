#!/bin/sh

raco make src/*.rkt
raco test -x .
