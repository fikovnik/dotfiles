#!/bin/sh

function do_paste {
    case "$(uname)" in
        Darwin*)
            exec pbpaste "$@"
            ;;
        Linux*)
            exec xsel -ib "$@"
            ;;
        *)
            echo "Unsupported OS: $(uname)"
            exit 1
            ;;
    esac
}

function do_copy {
    case "$(uname)" in
        Darwin*)
            exec pbcopy "$@"
            ;;
        Linux*)
            exec xsel -ob "$@"
            ;;
        *)
            echo "Unsupported OS: $(uname)"
            exit 1
            ;;
    esac
}

case "$1" in
    paste)
        do_paste "$@"
        ;;
    copy)
        do_copy "$@"
        ;;
    *)
        echo "Missing mode - either copy or paste"
        exit 1
        ;;
esac

