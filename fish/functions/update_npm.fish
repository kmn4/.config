function update_npm --wraps='npm update -g' --description 'alias update_npm npm update -g'
    which npm > /dev/null && npm update -g $argv; 
end
