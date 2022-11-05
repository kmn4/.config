#https://stackoverflow.com/questions/16407530/how-to-get-user-confirmation-in-fish-shell
function confirm
    while true
	read -l -P "$argv [y/N]" confirm
	switch $confirm
	    case Y y
		return 0
	    case '' N n
		return 1
	end
    end
end
