function update_stack --wraps='stack update && stack upgrade' --description 'alias update_stack stack update && stack upgrade'
    which stack > /dev/null && stack update && stack upgrade $argv; 
end
