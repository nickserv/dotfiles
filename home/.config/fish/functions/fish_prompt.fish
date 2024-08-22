function fish_prompt
    set -g fish_prompt_pwd_dir_length 0
    set_color $fish_color_cwd
    echo -n (prompt_pwd)
    set_color $fish_color_quote
    echo -n ' → '
end
