#!/run/current-system/sw/bin/sh

_infodir=$(mktemp -dt "info.XXXXXXXX" --tmpdir=/run/user/$(id -u))

_volume=$(mktemp -t "volume.XXXXXXXX" --tmpdir=$_infodir)
_music=$(mktemp -t "music.XXXXXXXX" --tmpdir=$_infodir)
_internet=$(mktemp -t "internet.XXXXXXXX" --tmpdir=$_infodir)

_volume_pipe=/tmp/volume_pipe
rm $_volume_pipe; mkfifo $_volume_pipe

_music_pipe=/tmp/music_pipe
rm $_music_pipe; mkfifo $_music_pipe;

# wait for xmobars to start
xmobar0_pid=$(ps ax | grep xmobar.hs | grep -v grep | cut -d '?' -f 1 | tr -d ' ')
while [ "$xmobar0_pid" = "" ]; do
	sleep 1;
	xmobar0_pid=$(ps ax | grep xmobar.hs | grep -v grep | cut -d '?' -f 1 | tr -d ' ')
done
xmobar1_pid=$(ps ax | grep xmobar-secondary.hs | grep -v grep | cut -d '?' -f 1 | tr -d ' ')
while [ "$xmobar0_pid" = "" ]; do
	sleep 1;
	xmobar0_pid=$(ps ax | grep xmobar.hs | grep -v grep | cut -d '?' -f 1 | tr -d ' ')
done

# write to the volume pipe
sh ~/.xmonad/scripts/volume_pipe.sh

# write to music pipe
echo "<fc=#268bd2><fn=1>Pure Silence</fn></fc>" | tee /tmp/music_pipe
echo "<fc=#268bd2><fn=1>Pure Silence</fn></fc>" | tee /tmp/music_pipe
echo "<fc=#268bd2><fn=1>Pure Silence</fn></fc>" | tee /tmp/music_pipe

