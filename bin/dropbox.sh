#!/bin/sh

function start {
    docker run \
       -d \
       --restart=always \
       --name=dropbox \
       -v $HOME/Dropbox:/dbox/Dropbox \
       -e DBOX_UID=$(id -u) \
       -e DBOX_GID=$(id -g) \
       janeczku/dropbox
}

case "$1" in
    start)
        if ! docker inspect dropbox > /dev/null 2>&1; then
            start
        else
            docker start dropbox
        fi
        ;;
    stop)
        docker stop dropbox
        ;;
    status)
        docker inspect -f "{{.State.Running}}" dropbox || echo "false"
        ;;
    *)
        echo "Usage: $0 <start|status|stop>"
        ;;
esac
