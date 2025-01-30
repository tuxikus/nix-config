{
  home.file.".config/elvish/rc.elv" = {
    text = ''
         use re
         use readline-binding

         set edit:insert:binding[Alt-Backspace] = $edit:kill-small-word-left~
         set edit:insert:binding[Alt-d] = $edit:kill-small-word-right~
         set edit:insert:binding[Alt-m] = $edit:-instant:start~
    '';       
  };
}
