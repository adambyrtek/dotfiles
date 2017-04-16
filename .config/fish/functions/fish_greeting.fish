function fish_greeting
    if which figlet > /dev/null
        figlet (hostname)
        echo
    end

    if which tmux > /dev/null
        tmux list-sessions
    end
end
