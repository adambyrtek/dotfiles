# .zprofile is sourced first for login shells only (even if not interactive)

# System profile and env variables (needed for Snap and Flatpak)
# See https://askubuntu.com/a/989485
[ -e /etc/profile ] && emulate sh -c 'source /etc/profile'
