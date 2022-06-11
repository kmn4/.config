function update_all
    update_apt
    update_cs
    update_brew
    update_npm
    update_rustup
    update_ghcup && update_stack
    update_winget
end
