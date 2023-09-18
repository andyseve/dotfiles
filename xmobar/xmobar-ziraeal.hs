Config
  { font = "xft:Fira Code Nerd Font Mono Bold 12",
    additionalFonts =
      [ "xft:Fira Code Nerd Font Mono Bold 10",
        "xft:Fira Code Nerd Font Mono Bold 30px"
      ],
    -- https://www.reddit.com/r/archlinux/comments/7n3uxw/font_awesome/
    -- use fc-list to find out correct font name
    bgColor = "#000000",
    fgColor = "#93a1a1",
    alpha = 150,
    position = TopH 40,
    lowerOnStart = True,
    hideOnStart = False,
    allDesktops = True,
    persistent = True,
    border = NoBorder,
    borderWidth = 0,
    commands =
      [ Run Date "%a %d %b %H:%M" "date" 20,
        Run PipeReader "/tmp/volume_pipe" "volume",
        Run Com "/home/stranger/.config/xmobar/scripts/internet.sh" [] "internet" 100,
        Run Com "/home/stranger/.config/xmobar/scripts/trayer_padding_icon.sh" [] "trayer" 10,
        Run
          BatteryN
          ["BAT0"]
          [ "-t",
            "<acstatus>",
            "-S",
            "Off",
            "-d",
            "0",
            "-m",
            "3",
            "-L",
            "10",
            "-H",
            "90",
            "-p",
            "3",
            "-W",
            "0",
            "-f",
            "\xf244\xf243\xf243\xf243\xf242\xf242\xf242\xf241\xf241\xf240",
            "--",
            "-P",
            "-a",
            "notify-send -u critical 'Battery running out!!!!!!'",
            "-A",
            "5",
            "-i",
            "\xf1e6",
            "-O",
            "<leftbar>  \xf1e6 <timeleft>",
            "-o",
            "<leftbar><timeleft>",
            "-H",
            "10",
            "-L",
            "7"
          ]
          50
          "batt0",
        Run UnsafeXMonadLog
      ],
    sepChar = "%",
    alignSep = "}{",
    template = " %UnsafeXMonadLog% } <action=xdotool key Super+c>%date%</action> { %volume% %internet% %batt0% %trayer% "
  }

-- vim:ft=haskell
