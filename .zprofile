# .zprofile is sourced first for login shells only (even if not interactive)

# Source system profile (required for Snap and Flatpak, see https://askubuntu.com/a/989485)
[ -e /etc/profile ] && emulate sh -c "source /etc/profile"

# Homebrew environment variables
[ -e /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
