#!/bin/sh

which_mon=0
case $(hostname) in
	opensuseme )
		which_mon=1 ;;
esac

trayer --edge top --align right --expand true --height 19 --widthtype percent --width 5 --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x44475a --monitor $which_mon&
