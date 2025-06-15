{
  home.file.".local/bin/fpass" = {
    text = ''
      #!/usr/bin/env bash
      
      set -euo pipefail
      trap 'unset selected_password selected_field field_value' EXIT
      
      finder=fzf
      selected_password=$(find ~/.password-store/ -type f -name "*.gpg" | sed 's|^'"$HOME"'/\.password-store/||; s|\.gpg||' | $finder)
      
      # If no selection, exit
      if [[ -z "$selected_password" ]]; then
          exit 0
      fi
      
      if [[ "$selected_password" == *"-otp" ]]; then
          pass otp -c "$selected_password"
      else
          selected_field=$(
              (
                  echo "password"
                  pass show "$selected_password" | sed '1d' | while IFS=":" read -r field value; do
                      echo "$field"
                  done
              ) | $finder
          )
      
          if [[ "$selected_field" == "password" ]]; then
              pass -c "$selected_password"
          else
              field_value=$(pass show "$selected_password" | grep -i "^$selected_field:" | cut -d ":" -f2- | xargs)
              echo -n "$field_value" | wl-copy
              echo "Copied $selected_field of $selected_password"
          fi
      fi
    '';

    executable = true;
  };
}
