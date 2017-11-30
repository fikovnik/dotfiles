#!/bin/sh

del_stopped(){
	local name=$1
	local state
	state=$(docker inspect --format "{{.State.Running}}" "$name" 2>/dev/null)

	if [[ "$state" == "false" ]]; then
		docker rm "$name"
	fi
}

del_stopped firefox

docker run -d \
	   --memory 2gb \
	   --net host \
	   --cpuset-cpus 0 \
	   -v /etc/localtime:/etc/localtime:ro \
	   -v /tmp/.X11-unix/X0:/tmp/.X11-unix/X0 \
	   -e "DISPLAY=:0" \
	   -e GDK_SCALE \
	   -e GDK_DPI_SCALE \
	   --device /dev/snd \
	   --device /dev/dri \
	   --name firefox \
	   jess/firefox "$@"

#	   -v "${HOME}/.firefox/cache:/root/.cache/mozilla" \
#	   -v "${HOME}/.firefox/mozilla:/root/.mozilla" \
#	   -v "${HOME}/Downloads:/root/Downloads" \
#	   -v "${HOME}/Pictures:/root/Pictures" \
#	   -v "${HOME}/Torrents:/root/Torrents" \
