function ecui --wraps=emacsclient\ -nw\ -a\ \'\' --description alias\ ecui\ emacsclient\ -nw\ -a\ \'\'
  emacsclient -nw -a '' $argv; 
end
