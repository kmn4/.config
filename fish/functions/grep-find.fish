function grep-find
    read -lP "extension: " ext
    read -lP "expression: " exp
    find . -type f -name "*.$ext" -exec grep --color=auto -nH -e $exp {} +
end
