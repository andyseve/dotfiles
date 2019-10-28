#!/bin/sh

printf '<action=`term -e mutt` button=1>'
printf '<icon=mail/mail.xpm/>'
printf '</action>'
printf " P:%s W:%s J:%s" "$(mailcheck "personal")" "$(mailcheck "work")" "$(mailcheck "junk")"
