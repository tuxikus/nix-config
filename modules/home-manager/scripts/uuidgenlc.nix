{
  home.file.".local/bin/uuidgenlc" = {
    text = ''
      #!/usr/bin/env bash
      
      uuidgen | tr A-Z a-z
    '';

    executable = true;
  };
}
