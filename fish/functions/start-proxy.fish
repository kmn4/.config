function start-proxy
enable-proxy
ssh -D 1080 $argv[1]
end
