function update_apt --wraps='which apt && sudo apt update && sudo apt upgrade && sudo apt autoremove' --wraps='which apt > /dev/null && sudo apt update && sudo apt upgrade && sudo apt autoremove' --description 'alias update_apt which apt > /dev/null && sudo apt update && sudo apt upgrade && sudo apt autoremove'
  which apt > /dev/null && sudo apt update && sudo apt upgrade && sudo apt autoremove $argv; 
end
