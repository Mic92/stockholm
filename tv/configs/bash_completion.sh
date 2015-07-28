
# Expand variable starting with tilde (~)
# We want to expand ~foo/... to /home/foo/... to avoid problems when
# word-to-complete starting with a tilde is fed to commands and ending up
# quoted instead of expanded.
# Only the first portion of the variable from the tilde up to the first slash
# (~../) is expanded.  The remainder of the variable, containing for example
# a dollar sign variable ($) or asterisk (*) is not expanded.
# Example usage:
#
#    $ v="~"; __expand_tilde_by_ref v; echo "$v"
#
# Example output:
#
#       v                  output
#    --------         ----------------
#    ~                /home/user
#    ~foo/bar         /home/foo/bar
#    ~foo/$HOME       /home/foo/$HOME
#    ~foo/a  b        /home/foo/a  b
#    ~foo/*           /home/foo/*
#
# @param $1  Name of variable (not the value of the variable) to expand
__expand_tilde_by_ref()
{
    # Does $1 start with tilde (~)?
    if [[ ${!1} == \~* ]]; then
        # Does $1 contain slash (/)?
        if [[ ${!1} == */* ]]; then
            # Yes, $1 contains slash;
            # 1: Remove * including and after first slash (/), i.e. "~a/b"
            #    becomes "~a".  Double quotes allow eval.
            # 2: Remove * before the first slash (/), i.e. "~a/b"
            #    becomes "b".  Single quotes prevent eval.
            #       +-----1----+ +---2----+
            eval $1="${!1/%\/*}"/'${!1#*/}'
        else
            # No, $1 doesn't contain slash
            eval $1="${!1}"
        fi
    fi
} # __expand_tilde_by_ref()


# Get the word to complete.
# This is nicer than ${COMP_WORDS[$COMP_CWORD]}, since it handles cases
# where the user is completing in the middle of a word.
# (For example, if the line is "ls foobar",
# and the cursor is here -------->   ^
# @param $1 string  Characters out of $COMP_WORDBREAKS which should NOT be
#     considered word breaks. This is useful for things like scp where
#     we want to return host:path and not only path, so we would pass the
#     colon (:) as $1 in this case.
# @param $2 integer  Index number of word to return, negatively offset to the
#     current word (default is 0, previous is 1), respecting the exclusions
#     given at $1.  For example, `_get_cword "=:" 1' returns the word left of
#     the current word, respecting the exclusions "=:".
# @deprecated  Use `_get_comp_words_by_ref cur' instead
# @see _get_comp_words_by_ref()
_get_cword()
{
    local LC_CTYPE=C
    local cword words
    __reassemble_comp_words_by_ref "$1" words cword

    # return previous word offset by $2
    if [[ ${2//[^0-9]/} ]]; then
        printf "%s" "${words[cword-$2]}"
    elif [[ "${#words[cword]}" -eq 0 || "$COMP_POINT" == "${#COMP_LINE}" ]]; then
        printf "%s" "${words[cword]}"
    else
        local i
        local cur="$COMP_LINE"
        local index="$COMP_POINT"
        for (( i = 0; i <= cword; ++i )); do
            while [[
                # Current word fits in $cur?
                "${#cur}" -ge ${#words[i]} &&
                # $cur doesn't match cword?
                "${cur:0:${#words[i]}}" != "${words[i]}"
            ]]; do
                # Strip first character
                cur="${cur:1}"
                # Decrease cursor position
                ((index--))
            done

            # Does found word matches cword?
            if [[ "$i" -lt "$cword" ]]; then
                # No, cword lies further;
                local old_size="${#cur}"
                cur="${cur#${words[i]}}"
                local new_size="${#cur}"
                index=$(( index - old_size + new_size ))
            fi
        done

        if [[ "${words[cword]:0:${#cur}}" != "$cur" ]]; then
            # We messed up! At least return the whole word so things
            # keep working
            printf "%s" "${words[cword]}"
        else
            printf "%s" "${cur:0:$index}"
        fi
    fi
} # _get_cword()


# Get word previous to the current word.
# This is a good alternative to `prev=${COMP_WORDS[COMP_CWORD-1]}' because bash4
# will properly return the previous word with respect to any given exclusions to
# COMP_WORDBREAKS.
# @deprecated  Use `_get_comp_words_by_ref cur prev' instead
# @see _get_comp_words_by_ref()
#
_get_pword()
{
    if [[ $COMP_CWORD -ge 1 ]]; then
        _get_cword "${@:-}" 1
    fi
}



# Complete variables.
# @return  True (0) if variables were completed,
#          False (> 0) if not.
_variables()
{
    if [[ $cur =~ ^(\$\{?)([A-Za-z0-9_]*)$ ]]; then
        [[ $cur == *{* ]] && local suffix=} || local suffix=
        COMPREPLY+=( $( compgen -P ${BASH_REMATCH[1]} -S "$suffix" -v -- \
            "${BASH_REMATCH[2]}" ) )
        return 0
    fi
    return 1
}

# Assign variable one scope above the caller
# Usage: local "$1" && _upvar $1 "value(s)"
# Param: $1  Variable name to assign value to
# Param: $*  Value(s) to assign.  If multiple values, an array is
#            assigned, otherwise a single value is assigned.
# NOTE: For assigning multiple variables, use '_upvars'.  Do NOT
#       use multiple '_upvar' calls, since one '_upvar' call might
#       reassign a variable to be used by another '_upvar' call.
# See: http://fvue.nl/wiki/Bash:_Passing_variables_by_reference
_upvar()
{
    if unset -v "$1"; then           # Unset & validate varname
        if (( $# == 2 )); then
            eval $1=\"\$2\"          # Return single value
        else
            eval $1=\(\"\${@:2}\"\)  # Return array
        fi
    fi
}

# Assign variables one scope above the caller
# Usage: local varname [varname ...] &&
#        _upvars [-v varname value] | [-aN varname [value ...]] ...
# Available OPTIONS:
#     -aN  Assign next N values to varname as array
#     -v   Assign single value to varname
# Return: 1 if error occurs
# See: http://fvue.nl/wiki/Bash:_Passing_variables_by_reference
_upvars()
{
    if ! (( $# )); then
        echo "${FUNCNAME[0]}: usage: ${FUNCNAME[0]} [-v varname"\
            "value] | [-aN varname [value ...]] ..." 1>&2
        return 2
    fi
    while (( $# )); do
        case $1 in
            -a*)
                # Error checking
                [[ ${1#-a} ]] || { echo "bash: ${FUNCNAME[0]}: \`$1': missing"\
                    "number specifier" 1>&2; return 1; }
                printf %d "${1#-a}" &> /dev/null || { echo "bash:"\
                    "${FUNCNAME[0]}: \`$1': invalid number specifier" 1>&2
                    return 1; }
                # Assign array of -aN elements
                [[ "$2" ]] && unset -v "$2" && eval $2=\(\"\${@:3:${1#-a}}\"\) &&
                shift $((${1#-a} + 2)) || { echo "bash: ${FUNCNAME[0]}:"\
                    "\`$1${2+ }$2': missing argument(s)" 1>&2; return 1; }
                ;;
            -v)
                # Assign single value
                [[ "$2" ]] && unset -v "$2" && eval $2=\"\$3\" &&
                shift 3 || { echo "bash: ${FUNCNAME[0]}: $1: missing"\
                "argument(s)" 1>&2; return 1; }
                ;;
            *)
                echo "bash: ${FUNCNAME[0]}: $1: invalid option" 1>&2
                return 1 ;;
        esac
    done
}

# @param $1 exclude  Characters out of $COMP_WORDBREAKS which should NOT be
#     considered word breaks. This is useful for things like scp where
#     we want to return host:path and not only path, so we would pass the
#     colon (:) as $1 in this case.
# @param $2 words  Name of variable to return words to
# @param $3 cword  Name of variable to return cword to
# @param $4 cur  Name of variable to return current word to complete to
# @see __reassemble_comp_words_by_ref()
__get_cword_at_cursor_by_ref()
{
    local cword words=()
    __reassemble_comp_words_by_ref "$1" words cword

    local i cur index=$COMP_POINT lead=${COMP_LINE:0:$COMP_POINT}
    # Cursor not at position 0 and not leaded by just space(s)?
    if [[ $index -gt 0 && ( $lead && ${lead//[[:space:]]} ) ]]; then
        cur=$COMP_LINE
        for (( i = 0; i <= cword; ++i )); do
            while [[
                # Current word fits in $cur?
                ${#cur} -ge ${#words[i]} &&
                # $cur doesn't match cword?
                "${cur:0:${#words[i]}}" != "${words[i]}"
            ]]; do
                # Strip first character
                cur="${cur:1}"
                # Decrease cursor position
                ((index--))
            done

            # Does found word match cword?
            if [[ $i -lt $cword ]]; then
                # No, cword lies further;
                local old_size=${#cur}
                cur="${cur#"${words[i]}"}"
                local new_size=${#cur}
                index=$(( index - old_size + new_size ))
            fi
        done
        # Clear $cur if just space(s)
        [[ $cur && ! ${cur//[[:space:]]} ]] && cur=
        # Zero $index if negative
        [[ $index -lt 0 ]] && index=0
    fi

    local "$2" "$3" "$4" && _upvars -a${#words[@]} $2 "${words[@]}" \
        -v $3 "$cword" -v $4 "${cur:0:$index}"
}

# Reassemble command line words, excluding specified characters from the
# list of word completion separators (COMP_WORDBREAKS).
# @param $1 chars  Characters out of $COMP_WORDBREAKS which should
#     NOT be considered word breaks. This is useful for things like scp where
#     we want to return host:path and not only path, so we would pass the
#     colon (:) as $1 here.
# @param $2 words  Name of variable to return words to
# @param $3 cword  Name of variable to return cword to
#
__reassemble_comp_words_by_ref()
{
    local exclude i j line ref
    # Exclude word separator characters?
    if [[ $1 ]]; then
        # Yes, exclude word separator characters;
        # Exclude only those characters, which were really included
        exclude="${1//[^$COMP_WORDBREAKS]}"
    fi

    # Default to cword unchanged
    eval $3=$COMP_CWORD
    # Are characters excluded which were former included?
    if [[ $exclude ]]; then
        # Yes, list of word completion separators has shrunk;
        line=$COMP_LINE
        # Re-assemble words to complete
        for (( i=0, j=0; i < ${#COMP_WORDS[@]}; i++, j++)); do
            # Is current word not word 0 (the command itself) and is word not
            # empty and is word made up of just word separator characters to
            # be excluded and is current word not preceded by whitespace in
            # original line?
            while [[ $i -gt 0 && ${COMP_WORDS[$i]} == +([$exclude]) ]]; do
                # Is word separator not preceded by whitespace in original line
                # and are we not going to append to word 0 (the command
                # itself), then append to current word.
                [[ $line != [$' \t']* ]] && (( j >= 2 )) && ((j--))
                # Append word separator to current or new word
                ref="$2[$j]"
                eval $2[$j]=\${!ref}\${COMP_WORDS[i]}
                # Indicate new cword
                [[ $i == $COMP_CWORD ]] && eval $3=$j
                # Remove optional whitespace + word separator from line copy
                line=${line#*"${COMP_WORDS[$i]}"}
                # Start new word if word separator in original line is
                # followed by whitespace.
                [[ $line == [$' \t']* ]] && ((j++))
                # Indicate next word if available, else end *both* while and
                # for loop
                (( $i < ${#COMP_WORDS[@]} - 1)) && ((i++)) || break 2
            done
            # Append word to current word
            ref="$2[$j]"
            eval $2[$j]=\${!ref}\${COMP_WORDS[i]}
            # Remove optional whitespace + word from line copy
            line=${line#*"${COMP_WORDS[i]}"}
            # Indicate new cword
            [[ $i == $COMP_CWORD ]] && eval $3=$j
        done
        [[ $i == $COMP_CWORD ]] && eval $3=$j
    else
        # No, list of word completions separators hasn't changed;
        eval $2=\( \"\${COMP_WORDS[@]}\" \)
    fi
} # __reassemble_comp_words_by_ref()


# If the word-to-complete contains a colon (:), left-trim COMPREPLY items with
# word-to-complete.
# With a colon in COMP_WORDBREAKS, words containing
# colons are always completed as entire words if the word to complete contains
# a colon.  This function fixes this, by removing the colon-containing-prefix
# from COMPREPLY items.
# The preferred solution is to remove the colon (:) from COMP_WORDBREAKS in
# your .bashrc:
#
#    # Remove colon (:) from list of word completion separators
#    COMP_WORDBREAKS=${COMP_WORDBREAKS//:}
#
# See also: Bash FAQ - E13) Why does filename completion misbehave if a colon
# appears in the filename? - http://tiswww.case.edu/php/chet/bash/FAQ
# @param $1 current word to complete (cur)
# @modifies global array $COMPREPLY
#
__ltrim_colon_completions()
{
    if [[ "$1" == *:* && "$COMP_WORDBREAKS" == *:* ]]; then
        # Remove colon-word prefix from COMPREPLY items
        local colon_word=${1%"${1##*:}"}
        local i=${#COMPREPLY[*]}
        while [[ $((--i)) -ge 0 ]]; do
            COMPREPLY[$i]=${COMPREPLY[$i]#"$colon_word"}
        done
    fi
} # __ltrim_colon_completions()


# NOTE: Using this function as a helper function is deprecated.  Use
#       `_known_hosts_real' instead.
_known_hosts()
{
    local cur prev words cword
    _init_completion -n : || return

    # NOTE: Using `_known_hosts' as a helper function and passing options
    #       to `_known_hosts' is deprecated: Use `_known_hosts_real' instead.
    local options
    [[ "$1" == -a || "$2" == -a ]] && options=-a
    [[ "$1" == -c || "$2" == -c ]] && options+=" -c"
    _known_hosts_real $options -- "$cur"
} # _known_hosts()


# Helper function for completing _known_hosts.
# This function performs host completion based on ssh's config and known_hosts
# files, as well as hostnames reported by avahi-browse if
# COMP_KNOWN_HOSTS_WITH_AVAHI is set to a non-empty value.  Also hosts from
# HOSTFILE (compgen -A hostname) are added, unless
# COMP_KNOWN_HOSTS_WITH_HOSTFILE is set to an empty value.
# Usage: _known_hosts_real [OPTIONS] CWORD
# Options:  -a             Use aliases
#           -c             Use `:' suffix
#           -F configfile  Use `configfile' for configuration settings
#           -p PREFIX      Use PREFIX
# Return: Completions, starting with CWORD, are added to COMPREPLY[]
_known_hosts_real()
{
    local configfile flag prefix
    local cur curd awkcur user suffix aliases i host
    local -a kh khd config

    local OPTIND=1
    while getopts "acF:p:" flag "$@"; do
        case $flag in
            a) aliases='yes' ;;
            c) suffix=':' ;;
            F) configfile=$OPTARG ;;
            p) prefix=$OPTARG ;;
        esac
    done
    [[ $# -lt $OPTIND ]] && echo "error: $FUNCNAME: missing mandatory argument CWORD"
    cur=${!OPTIND}; let "OPTIND += 1"
    [[ $# -ge $OPTIND ]] && echo "error: $FUNCNAME("$@"): unprocessed arguments:"\
    $(while [[ $# -ge $OPTIND ]]; do printf '%s\n' ${!OPTIND}; shift; done)

    [[ $cur == *@* ]] && user=${cur%@*}@ && cur=${cur#*@}
    kh=()

    # ssh config files
    if [[ -n $configfile ]]; then
        [[ -r $configfile ]] && config+=( "$configfile" )
    else
        for i in /etc/ssh/ssh_config ~/.ssh/config ~/.ssh2/config; do
            [[ -r $i ]] && config+=( "$i" )
        done
    fi

    # Known hosts files from configs
    if [[ ${#config[@]} -gt 0 ]]; then
        local OIFS=$IFS IFS=$'\n' j
        local -a tmpkh
        # expand paths (if present) to global and user known hosts files
        # TODO(?): try to make known hosts files with more than one consecutive
        #          spaces in their name work (watch out for ~ expansion
        #          breakage! Alioth#311595)
        tmpkh=( $( awk 'sub("^[ \t]*([Gg][Ll][Oo][Bb][Aa][Ll]|[Uu][Ss][Ee][Rr])[Kk][Nn][Oo][Ww][Nn][Hh][Oo][Ss][Tt][Ss][Ff][Ii][Ll][Ee][ \t]+", "") { print $0 }' "${config[@]}" | sort -u ) )
        IFS=$OIFS
        for i in "${tmpkh[@]}"; do
            # First deal with quoted entries...
            while [[ $i =~ ^([^\"]*)\"([^\"]*)\"(.*)$ ]]; do
                i=${BASH_REMATCH[1]}${BASH_REMATCH[3]}
                j=${BASH_REMATCH[2]}
                __expand_tilde_by_ref j # Eval/expand possible `~' or `~user'
                [[ -r $j ]] && kh+=( "$j" )
            done
            # ...and then the rest.
            for j in $i; do
                __expand_tilde_by_ref j # Eval/expand possible `~' or `~user'
                [[ -r $j ]] && kh+=( "$j" )
            done
        done
    fi


    if [[ -z $configfile ]]; then
        # Global and user known_hosts files
        for i in /etc/ssh/ssh_known_hosts /etc/ssh/ssh_known_hosts2 \
            /etc/known_hosts /etc/known_hosts2 ~/.ssh/known_hosts \
            ~/.ssh/known_hosts2; do
            [[ -r $i ]] && kh+=( "$i" )
        done
        for i in /etc/ssh2/knownhosts ~/.ssh2/hostkeys; do
            [[ -d $i ]] && khd+=( "$i"/*pub )
        done
    fi

    # If we have known_hosts files to use
    if [[ ${#kh[@]} -gt 0 || ${#khd[@]} -gt 0 ]]; then
        # Escape slashes and dots in paths for awk
        awkcur=${cur//\//\\\/}
        awkcur=${awkcur//\./\\\.}
        curd=$awkcur

        if [[ "$awkcur" == [0-9]*[.:]* ]]; then
            # Digits followed by a dot or a colon - just search for that
            awkcur="^$awkcur[.:]*"
        elif [[ "$awkcur" == [0-9]* ]]; then
            # Digits followed by no dot or colon - search for digits followed
            # by a dot or a colon
            awkcur="^$awkcur.*[.:]"
        elif [[ -z $awkcur ]]; then
            # A blank - search for a dot, a colon, or an alpha character
            awkcur="[a-z.:]"
        else
            awkcur="^$awkcur"
        fi

        if [[ ${#kh[@]} -gt 0 ]]; then
            # FS needs to look for a comma separated list
            COMPREPLY+=( $( awk 'BEGIN {FS=","}
            /^\s*[^|\#]/ {
            sub("^@[^ ]+ +", ""); \
            sub(" .*$", ""); \
            for (i=1; i<=NF; ++i) { \
            sub("^\\[", "", $i); sub("\\](:[0-9]+)?$", "", $i); \
            if ($i !~ /[*?]/ && $i ~ /'"$awkcur"'/) {print $i} \
            }}' "${kh[@]}" 2>/dev/null ) )
        fi
        if [[ ${#khd[@]} -gt 0 ]]; then
            # Needs to look for files called
            # .../.ssh2/key_22_<hostname>.pub
            # dont fork any processes, because in a cluster environment,
            # there can be hundreds of hostkeys
            for i in "${khd[@]}" ; do
                if [[ "$i" == *key_22_$curd*.pub && -r "$i" ]]; then
                    host=${i/#*key_22_/}
                    host=${host/%.pub/}
                    COMPREPLY+=( $host )
                fi
            done
        fi

        # apply suffix and prefix
        for (( i=0; i < ${#COMPREPLY[@]}; i++ )); do
            COMPREPLY[i]=$prefix$user${COMPREPLY[i]}$suffix
        done
    fi

    # append any available aliases from config files
    if [[ ${#config[@]} -gt 0 && -n "$aliases" ]]; then
        local hosts=$( sed -ne 's/^[ \t]*[Hh][Oo][Ss][Tt]\([Nn][Aa][Mm][Ee]\)\{0,1\}['"$'\t '"']\{1,\}\([^#*?]*\)\(#.*\)\{0,1\}$/\2/p' "${config[@]}" )
        COMPREPLY+=( $( compgen -P "$prefix$user" \
            -S "$suffix" -W "$hosts" -- "$cur" ) )
    fi

    # Add hosts reported by avahi-browse, if desired and it's available.
    if [[ ${COMP_KNOWN_HOSTS_WITH_AVAHI:-} ]] && \
        type avahi-browse &>/dev/null; then
        # The original call to avahi-browse also had "-k", to avoid lookups
        # into avahi's services DB. We don't need the name of the service, and
        # if it contains ";", it may mistify the result. But on Gentoo (at
        # least), -k wasn't available (even if mentioned in the manpage) some
        # time ago, so...
        COMPREPLY+=( $( compgen -P "$prefix$user" -S "$suffix" -W \
            "$( avahi-browse -cpr _workstation._tcp 2>/dev/null | \
                 awk -F';' '/^=/ { print $7 }' | sort -u )" -- "$cur" ) )
    fi

    # Add hosts reported by ruptime.
    COMPREPLY+=( $( compgen -W \
        "$( ruptime 2>/dev/null | awk '!/^ruptime:/ { print $1 }' )" \
        -- "$cur" ) )

    # Add results of normal hostname completion, unless
    # `COMP_KNOWN_HOSTS_WITH_HOSTFILE' is set to an empty value.
    if [[ -n ${COMP_KNOWN_HOSTS_WITH_HOSTFILE-1} ]]; then
        COMPREPLY+=(
            $( compgen -A hostname -P "$prefix$user" -S "$suffix" -- "$cur" ) )
    fi

    __ltrim_colon_completions "$prefix$user$cur"

    return 0
} # _known_hosts_real()


# Get the word to complete and optional previous words.
# This is nicer than ${COMP_WORDS[$COMP_CWORD]}, since it handles cases
# where the user is completing in the middle of a word.
# (For example, if the line is "ls foobar",
# and the cursor is here -------->   ^
# Also one is able to cross over possible wordbreak characters.
# Usage: _get_comp_words_by_ref [OPTIONS] [VARNAMES]
# Available VARNAMES:
#     cur         Return cur via $cur
#     prev        Return prev via $prev
#     words       Return words via $words
#     cword       Return cword via $cword
#
# Available OPTIONS:
#     -n EXCLUDE  Characters out of $COMP_WORDBREAKS which should NOT be
#                 considered word breaks. This is useful for things like scp
#                 where we want to return host:path and not only path, so we
#                 would pass the colon (:) as -n option in this case.
#     -c VARNAME  Return cur via $VARNAME
#     -p VARNAME  Return prev via $VARNAME
#     -w VARNAME  Return words via $VARNAME
#     -i VARNAME  Return cword via $VARNAME
#
# Example usage:
#
#    $ _get_comp_words_by_ref -n : cur prev
#
_get_comp_words_by_ref()
{
    local exclude flag i OPTIND=1
    local cur cword words=()
    local upargs=() upvars=() vcur vcword vprev vwords

    while getopts "c:i:n:p:w:" flag "$@"; do
        case $flag in
            c) vcur=$OPTARG ;;
            i) vcword=$OPTARG ;;
            n) exclude=$OPTARG ;;
            p) vprev=$OPTARG ;;
            w) vwords=$OPTARG ;;
        esac
    done
    while [[ $# -ge $OPTIND ]]; do
        case ${!OPTIND} in
            cur)   vcur=cur ;;
            prev)  vprev=prev ;;
            cword) vcword=cword ;;
            words) vwords=words ;;
            *) echo "bash: $FUNCNAME(): \`${!OPTIND}': unknown argument" \
                1>&2; return 1
        esac
        let "OPTIND += 1"
    done

    __get_cword_at_cursor_by_ref "$exclude" words cword cur

    [[ $vcur   ]] && { upvars+=("$vcur"  ); upargs+=(-v $vcur   "$cur"  ); }
    [[ $vcword ]] && { upvars+=("$vcword"); upargs+=(-v $vcword "$cword"); }
    [[ $vprev && $cword -ge 1 ]] && { upvars+=("$vprev" ); upargs+=(-v $vprev
        "${words[cword - 1]}"); }
    [[ $vwords ]] && { upvars+=("$vwords"); upargs+=(-a${#words[@]} $vwords
        "${words[@]}"); }

    (( ${#upvars[@]} )) && local "${upvars[@]}" && _upvars "${upargs[@]}"
}

# Initialize completion and deal with various general things: do file
# and variable completion where appropriate, and adjust prev, words,
# and cword as if no redirections exist so that completions do not
# need to deal with them.  Before calling this function, make sure
# cur, prev, words, and cword are local, ditto split if you use -s.
#
# Options:
#     -n EXCLUDE  Passed to _get_comp_words_by_ref -n with redirection chars
#     -e XSPEC    Passed to _filedir as first arg for stderr redirections
#     -o XSPEC    Passed to _filedir as first arg for other output redirections
#     -i XSPEC    Passed to _filedir as first arg for stdin redirections
#     -s          Split long options with _split_longopt, implies -n =
# @return  True (0) if completion needs further processing,
#          False (> 0) no further processing is necessary.
#
_init_completion()
{
    local exclude= flag outx errx inx OPTIND=1

    while getopts "n:e:o:i:s" flag "$@"; do
        case $flag in
            n) exclude+=$OPTARG ;;
            e) errx=$OPTARG ;;
            o) outx=$OPTARG ;;
            i) inx=$OPTARG ;;
            s) split=false ; exclude+== ;;
        esac
    done

    # For some reason completion functions are not invoked at all by
    # bash (at least as of 4.1.7) after the command line contains an
    # ampersand so we don't get a chance to deal with redirections
    # containing them, but if we did, hopefully the below would also
    # do the right thing with them...

    COMPREPLY=()
    local redir="@(?([0-9])<|?([0-9&])>?(>)|>&)"
    _get_comp_words_by_ref -n "$exclude<>&" cur prev words cword

    # Complete variable names.
    _variables && return 1

    # Complete on files if current is a redirect possibly followed by a
    # filename, e.g. ">foo", or previous is a "bare" redirect, e.g. ">".
    if [[ $cur == $redir* || $prev == $redir ]]; then
        local xspec
        case $cur in
            2'>'*) xspec=$errx ;;
            *'>'*) xspec=$outx ;;
            *'<'*) xspec=$inx ;;
            *)
                case $prev in
                    2'>'*) xspec=$errx ;;
                    *'>'*) xspec=$outx ;;
                    *'<'*) xspec=$inx ;;
                esac
                ;;
        esac
        cur="${cur##$redir}"
        _filedir $xspec
        return 1
    fi

    # Remove all redirections so completions don't have to deal with them.
    local i skip
    for (( i=1; i < ${#words[@]}; )); do
        if [[ ${words[i]} == $redir* ]]; then
            # If "bare" redirect, remove also the next word (skip=2).
            [[ ${words[i]} == $redir ]] && skip=2 || skip=1
            words=( "${words[@]:0:i}" "${words[@]:i+skip}" )
            [[ $i -le $cword ]] && cword=$(( cword - skip ))
        else
            i=$(( ++i ))
        fi
    done

    [[ $cword -le 0 ]] && return 1
    prev=${words[cword-1]}

    [[ ${split-} ]] && _split_longopt && split=true

    return 0
}

# Try to complete -o SubOptions=
#
# Returns 0 if the completion was handled or non-zero otherwise.
_ssh_suboption_check()
{
    # Get prev and cur words without splitting on =
    local cureq=`_get_cword :=` preveq=`_get_pword :=`
    if [[ $cureq == *=* && $preveq == -o ]]; then
        _ssh_suboption $cureq
        return $?
    fi
    return 1
}

_complete_ssh()
{
    local cur prev words cword
    _init_completion -n : || return

    local configfile
    local -a config

    _ssh_suboption_check && return 0

    case $prev in
        -F|-i|-S)
            _filedir
            return 0
            ;;
        -c)
            _ssh_ciphers
            return 0
            ;;
        -m)
            _ssh_macs
            return 0
            ;;
        -l)
            COMPREPLY=( $( compgen -u -- "$cur" ) )
            return 0
            ;;
        -O)
            COMPREPLY=( $( compgen -W 'check forward exit stop' -- "$cur" ) )
            return 0
            ;;
        -o)
            _ssh_options
            return 0
            ;;
        -w)
            _available_interfaces
            return 0
            ;;
        -b)
            _ip_addresses
            return 0
            ;;
        -D|-e|-I|-L|-p|-R|-W)
            return 0
            ;;
    esac

    if [[ "$cur" == -F* ]]; then
        cur=${cur#-F}
        _filedir
        # Prefix completions with '-F'
        COMPREPLY=( "${COMPREPLY[@]/#/-F}" )
        cur=-F$cur  # Restore cur
    elif [[ "$cur" == -* ]]; then
        COMPREPLY=( $( compgen -W '$( _parse_usage "$1" )' -- "$cur" ) )
    else
        # Search COMP_WORDS for '-F configfile' or '-Fconfigfile' argument
        set -- "${words[@]}"
        while [[ $# -gt 0 ]]; do
            if [[ $1 == -F* ]]; then
                if [[ ${#1} -gt 2 ]]; then
                    configfile="$(dequote "${1:2}")"
                else
                    shift
                    [[ $1 ]] && configfile="$(dequote "$1")"
                fi
                break
            fi
            shift
        done
        _known_hosts_real -a -F "$configfile" "$cur"
        if [[ $cword -ne 1 ]]; then
            compopt -o filenames
            COMPREPLY+=( $( compgen -c -- "$cur" ) )
        fi
    fi

    return 0
} &&
shopt -u hostcomplete && complete -F _complete_ssh ssh
