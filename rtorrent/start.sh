#! /usr/bin/bash

while [[ "$(curl http://ipecho.net/plain; echo)" != "37.72.175.72" ]]; do
	sleep 1;
done

if [[$(curl http://ipecho.net/plain; echo)" == "37.72.175.72" ]]; then
	/usr/bin/tmux -L rtorrent -S /tmp/rtorrent new-session -s rtorrent -n rtorrent -d "rtorrent -n -o import=/media/torrent/.rtorrent.rc"
fi
