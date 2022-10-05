function setup-mozc --wraps='/usr/lib/mozc/mozc_tool --mode=config_dialog' --description 'alias setup-mozc /usr/lib/mozc/mozc_tool --mode=config_dialog'
  /usr/lib/mozc/mozc_tool --mode=config_dialog $argv; 
end
