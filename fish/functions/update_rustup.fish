function update_rustup --wraps='rustup update' --description 'alias update_rustup rustup update'
    which rustup > /dev/null && rustup update $argv; 
end
