#!/usr/bin/env awk -f
BEGIN {
    print "-module(pgsql_errcodes)."
    print "-include(\"pgsql.hrl\")."
    print "-export([to_name/1])."
    print 
    print "to_name(#error{code=Code}) -> to_name(Code);"
}
NF == 4 && \
$1 ~ /[^\s]{5}/ && \
$2 ~ /[EWS]/ \
{
    printf("to_name(<<\"%s\">>) -> %s;\n", $1, $4)
}
END {
    print "to_name(_) -> undefined."
}
