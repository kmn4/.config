function update_emacs_packages --wraps=which\ emacs\ \>\ /dev/null\ \&\&\ emacsclient\ -e\ \'\(package-upgrade-packages-with-logging\)\' --description alias\ update_emacs_packages\ which\ emacs\ \>\ /dev/null\ \&\&\ emacsclient\ -e\ \'\(package-upgrade-packages-with-logging\)\'
  which emacs > /dev/null && emacsclient -e '(package-upgrade-packages-with-logging)' $argv;
end
