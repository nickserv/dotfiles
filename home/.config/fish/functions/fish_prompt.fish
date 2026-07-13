function fish_prompt
    set -g fish_prompt_pwd_dir_length 0

    # Shorten Google Drive paths for readability
    set -l real_pwd (prompt_pwd | string replace "~/Library/CloudStorage/GoogleDrive-thenickperson@gmail.com/My Drive" "\$DRIVE")

    set_color $fish_color_cwd
    echo -n $real_pwd
    set_color $fish_color_quote
    echo -n ' → '
end
