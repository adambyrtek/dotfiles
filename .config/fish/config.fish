set -x EDITOR vim
set PATH "$HOME/Dev/bin" $PATH
set CDPATH . "$HOME/Dev"

if status --is-interactive
    set -e fish_user_abbreviations

    abbr -a l less
    abbr -a g egrep
    abbr -a py python
    abbr -a ipy ipython
    abbr -a s sudo

    # Git
    abbr -a ga git add
    abbr -a gb git branch -v
    abbr -a gc git commit -v
    abbr -a gco git checkout
    abbr -a gcom git checkout master
    abbr -a gd git diff
    abbr -a gf git fetch --all -p
    abbr -a gh git help
    abbr -a gm git merge
    abbr -a gp git push
    abbr -a gl git pull --all -p
    abbr -a glg git log --graph
    abbr -a gr git remote -v
    abbr -a gst git status -sb
    abbr -a gt git tag

    function __fzf_reverse_isearch
        builtin history | fzf +s --tiebreak=index --toggle-sort=ctrl-r -q (commandline) | read -z select
        if not test -z $select
            commandline -rb $select
            commandline -f repaint
        end
    end
end
