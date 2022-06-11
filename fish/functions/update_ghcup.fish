function update_ghcup --wraps='ghcup upgrade' --description 'alias update_ghcup ghcup upgrade'
    which gchup > /dev/null && ghcup upgrade $argv; 
end
