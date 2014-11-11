#! /bin/sh

# Tool to lock trees

prgname="locktree"
. /usr/local/share/shell/sistim.sh || { echo "can't open library"; exit 1; }

user="nobody"
group="nobody"

main() {
    action=$1
    path=$2

    if [ -z "$action" ]; then
        warning "please provide an action"
        usage
        exit 2
    fi

    case $action in
        lock)
            #chown "$user:$group" "$path"
            warning "locking"
            ;;
        unlock)
            current_user=$(id -rnu)
            current_group=$(id -rng)
            echo "would chown $current_user:$current_group"
            warning "unlocking"
            ;;
        *)
            fatal "unknown action: '$action'"
    esac
}

usage() {
    echo "usage: locktree ACTION PATH"
    echo "ACTION is 'lock' or 'unlock'.  PATH can be a directory or a file."
}

main "$@"
