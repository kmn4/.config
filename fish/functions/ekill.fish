function ekill
set -l sig INT
if set -q argv[1]
set sig $argv[1]
end
kill -s$sig (ps -e | awk '{ if ($4 == "emacs") print $1; }')
end
