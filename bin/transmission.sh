#!/bin/sh

name=transmission

# TODO: refactor to some docker utils
function rm_stopped {
	local name=$1
	local state
	state=$(status "$name")

	if [[ "$state" == "false" ]]; then
		docker rm "$name"
	fi
}

function start {
    local password=$(pass show internet/windscribe-vpn | head -1)
    local username=$(pass show internet/windscribe-vpn | tail -1 | sed 's/username: //g')

    docker run --cap-add=NET_ADMIN --device=/dev/net/tun -d \
           -v $HOME/NoBackup/transmission/:/data \
           -v /etc/localtime:/etc/localtime:ro \
           -e "OPENVPN_PROVIDER=WINDSCRIBE" \
           -e "OPENVPN_CONFIG=Germany-udp" \
           -e "OPENVPN_USERNAME=$username" \
           -e "OPENVPN_PASSWORD=$password" \
           -e "PUID=$(id -u)" \
           -e "PGID=$(id -g)" \
           -e "TRANSMISSION_WEB_UI=combustion" \
           --dns 8.8.8.8 \
           --dns 8.8.4.4 \
           --log-driver json-file \
           --log-opt max-size=10m \
           --name "$name" \
           -p 9091:9091 \
           haugene/transmission-openvpn
}

function status {
    docker inspect -f "{{.State.Running}}" "$name" || echo "false"
}

case "$1" in
    start)
        rm_stopped "$name"
        start
        ;;
    stop)
        docker stop "$name"
        ;;
    restart)
        [ "$(status)" = "true" ] && docker kill "$name"
        docker rm "$name"
        start
        ;;
    status)
        status
        ;;
    *)
        echo "Usage: $0 <start|stop|restart|status>"
        ;;
esac
