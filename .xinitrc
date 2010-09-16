# XFCE manages settings itself
if [[ "$1" == "xfce4" ]]; then
    exec startxfce4
fi

# Polish layout and remap Caps-Lock to Ctrl
setxkbmap -layout pl -option ctrl:nocaps

# Faster keyboard repeat rate
xset r rate 300 70

# Black background by default
xsetroot -bg black
xsetroot -cursor_name left_ptr

# Multimedia key bindings
xbindkeys &

# Set background with Nitogen
nitrogen --restore

# Hide pointer when not active
unclutter &

# Startup applications
#blueman-applet &
#nm-applet &
wicd-client &

# Session name passed by Slim
case "$1" in
    openbox)
        # Openbox
        pypanel &
        xcompmgr -cC -t-3 -l-5 -r5 &
        exec openbox
        ;;
    xmonad)
        #trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --height 12 --alpha 0 --tint 0x222222 &
        exec xmonad
        ;;
    *)
        exec i3
        ;;
esac
