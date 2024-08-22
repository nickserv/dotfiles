set -x EDITOR 'code --wait'
set -x PYTHONSTARTUP ~/.pythonrc
set -x RIPGREP_CONFIG_PATH ~/.ripgreprc
set -x RUST_BACKTRACE ''

alias acid "curl -sL https://github.com/nodejs/node-gyp/raw/main/macOS_Catalina_acid_test.sh | bash"
