#! /bin/sh

prgname=anankasm

main() {
    [ $# -eq 0 ] && die "subcommand required, use -h for help"

    cmd=$1
    shift

    prefix=/usr/local
    execpath="${prefix}/lib/anankasm/${cmd}"

    if [ -x "$execpath" ]; then
        exec "$execpath" "$@"
    else
        die "cannot find subcommand '$cmd'"
    fi
}

die() {
    echo "${prgname}: $@" >&2
    exit 1
}

main "$@"
