function egui --wraps=emacsclient\ -cn\ -a\ \'\' --description alias\ egui\ emacsclient\ -cn\ -a\ \'\'
  emacsclient -cn -a '' $argv; 
end
