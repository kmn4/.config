function update_winget --wraps=which\ powershell.exe\ \>\ /dev/null\ \&\&\ powershell.exe\ -Command\ \'winget\ upgrade\ --all\' --wraps=which\ powershell.exe\ \>\ /dev/null\ \&\&\ powershell.exe\ -Command\ \'winget\ upgrade\' --description alias\ update_winget\ which\ powershell.exe\ \>\ /dev/null\ \&\&\ powershell.exe\ -Command\ \'winget\ upgrade\'
  which powershell.exe > /dev/null && powershell.exe -Command 'winget upgrade' $argv; 
end
