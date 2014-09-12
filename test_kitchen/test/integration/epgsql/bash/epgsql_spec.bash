#!/usr/bin/env bash

set -e

# we compile as root
make -C /root/epgsql clean compile_tests

# then run the tests as postgres
su - postgres -c "make -C /root/epgsql test"
