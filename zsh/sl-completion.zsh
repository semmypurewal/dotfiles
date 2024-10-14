_sl_complete() {
    local curcontext="$curcontext" state line
    typeset -A opt_args

    # Define a list of sl subcommands
    local -a subcommands
    subcommands=(
        "clone" "log" "rebase" "checkout" "co" "pull" "pr" "branch" "histedit"
    )

    # Handle autocompletion
    _arguments -C \
        '1:subcommand:->subcommand' \
        '*: :->args'

    case $state in
        subcommand)
            # Complete subcommands
            compadd $subcommands
            ;;
        args)
            # Handle cases when words[0] is empty (shift the indices)
            local command=${words[1]}
            local subcommand=${words[2]}

            # Determine the last non-empty argument
            local last_arg
            if [[ -z ${words[-1]} ]]; then
                # If the last word is an empty string (space at the end), check the previous word
                last_arg=${words[-2]}
            else
                last_arg=${words[-1]}
            fi

            if [[ "$subcommand" == "rebase" ]]; then
                # If the last argument is -s or -d, autocomplete the SHA
                if [[ "$last_arg" == "-s" || "$last_arg" == "-d" ]]; then
                    local shas=($(sl log --all --limit 100 --template '{node|short}\n'))
                    compadd $shas
                else
                    # If neither -s nor -d is the last argument, suggest the options
                    compadd -s -d
                fi
            elif [[ "$subcommand" == "checkout" || "$subcommand" == "co" || "$subcommand" == "histedit" || "$subcommand" == "pull" ]]; then
                # Autocomplete SHAs for other subcommands
                local shas=($(sl log --all --limit 100 --template '{node|short}\n'))
                compadd $shas
            fi
            ;;
    esac
}

# Register the autocompletion function for sl
compdef _sl_complete sl
