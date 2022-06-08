#!/run/current-system/sw/bin/sh

_music_pipe=/tmp/music_pipe
rm $_music_pipe; mkfifo $_music_pipe;


_volume_pipe=/tmp/volume_pipe
rm $_volume_pipe; mkfifo $_volume_pipe

# echo into /tmp/music_pipe multiple times so that there is something to consume it
sleep 10;
echo "<fc=#268bd2><fn=1>Pure Silence</fn></fc>" | tee /tmp/music_pipe
echo "<fc=#268bd2><fn=1>Pure Silence</fn></fc>" | tee /tmp/music_pipe
echo "<fc=#268bd2><fn=1>Pure Silence</fn></fc>" | tee /tmp/music_pipe

# run volume command to fill the pipe
sh ~/.xmonad/scripts/volume_pipe.sh


