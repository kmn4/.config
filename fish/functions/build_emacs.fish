function build_emacs
    set -l revision $argv[1]
    set -l configure_options \
	--with-cairo \
	--with-json \
	--with-native-compilation=aot \
	--with-xwidgets
    if not set -q __emacs_repo
	echo 'set $__emacs_repo first.'
	return 1
    end
    cd $__emacs_repo

    if test -z $revision # build master branch
        set revision master
        git checkout $revision && git pull
    end

    git checkout $revision && ./autogen.sh && ./configure $configure_options && make -j 8

    if [ $status -eq 0 ]
	confirm 'install?' && sudo make install
	prevd
    end
end
