function update_cs --wraps='which cs > /dev/null && cs update' --description 'alias update_cs which cs > /dev/null && cs update'
  which cs > /dev/null && cs update $argv; 
end
