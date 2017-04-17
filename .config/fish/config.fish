set -x EDITOR vim
set PATH "$HOME/Dev/bin" $PATH
set CDPATH . "$HOME/Dev"

abbr -a ga git add
abbr -a gb git branch -v
abbr -a gc git commit -v
abbr -a gco git checkout
abbr -a gcom git checkout master
abbr -a gd git diff
abbr -a gf git fetch -p
abbr -a gh git help
abbr -a gm git merge
abbr -a gp git push
abbr -a gl git pull -p
abbr -a glg git log --graph
abbr -a gr git remote -v
abbr -a gst git status -sb
abbr -a gt git tag
