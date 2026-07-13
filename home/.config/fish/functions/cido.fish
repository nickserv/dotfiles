function cido
    if set -q argv[1]
        env CI=true $argv
    else
        echo "usage: cido <command> [arg ...]"
    end
end
