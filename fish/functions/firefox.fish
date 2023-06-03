function firefox
    if which firefox; firefox $argv; else; win-firefox $argv; end
end
