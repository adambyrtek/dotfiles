# ~/.zlogin
# Sourced for login shells only

if which figlet > /dev/null; then
    figlet $(hostname)
fi
uptime
