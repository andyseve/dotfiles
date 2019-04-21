#! /usr/bin/bash

VPNIP=192.210.227.238
MYIP=$(curl -s http://ipecho.net/plain; echo)

if [[ "$MYIP" != "$VPNIP" ]]; then
	echo "unsafe ip $MYIP, check vpn"
fi

while [[ "$MYIP" != "$VPNIP" ]]; do
	sleep 1;
	MYIP=$(curl -s http://ipecho.net/plain; echo)
done

MYIP=$(curl -s http://ipecho.net/plain; echo)

if [[ "$MYIP" == "$VPNIP" ]]; then
	/usr/bin/tmux -L rtorrent -S /tmp/rtorrent new-session -s rtorrent -n rtorrent -d "rtorrent -n -o import=/media/torrents/.rtorrent.rc"
fi
