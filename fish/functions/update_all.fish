function update_all
    update_apt
    update_cs
    update_emacs_packages
    update_brew
    update_npm
    functions fisher && fisher update
    update_rustup
    update_ghcup && update_stack
    update_winget
end
