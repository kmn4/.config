function build_emacs_master
    set -l configure_options \
	--with-cairo \
	--with-json \
	--with-native-compilation \
	--with-xwidgets
    if not set -q __emacs_repo
	echo 'set $__emacs_repo first.'
	return 1
    end
    cd $__emacs_repo

    git switch master && git pull && ./autogen.sh && ./configure $configure_options && make -j 8

    if [ $status -eq 0 ]
	confirm 'install?' && sudo make install
	prevd
    end
end
