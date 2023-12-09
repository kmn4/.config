function ekill
    set -l sig INT
    not set -q argv[1] || set sig $argv[1]
    pgrep -x "emacs" | xargs kill -s$sig
end
