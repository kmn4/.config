function cdtmp --wraps='cd (mktemp -d)' --description 'alias cdtmp cd (mktemp -d)'
  cd (mktemp -d) $argv; 
end
