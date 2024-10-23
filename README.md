Primitive obfuscator for commands or scripts.

```console
$ sh build.sh
$ ./slurb 'for x in {0..2}; do echo hello-${x}; done'
$ sh slurbed.sh
hello-1
hello-2
hello-3
$ cat ./slurbed.sh
declare -a PvQ=( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 );declare -A ZyEd=( ["f"]=2 ["o"]=337583319 ["r"]=5 [" "]=133521430133 ["x"]=1529 ["i"]=17 ["n"]=3287 ["{"]=3973 ["0"]=31 ["."]=1517 ["2"]=43 ["}"]=7003 [";"]=8003 ["d"]=9943 ["e"]=1345901 ["c"]=79 ["h"]=8383 ["l"]=11663 ["-"]=127 ["$"]=131 );for m in ${PvQ[@]}; do for wF in "${!ZyEd[@]}"; do if [[ $(echo "${ZyEd["$wF"]} % $m" | bc ) -eq 0 ]]; then if [[ $wF == '\n' ]]; then wF=$'\n';fi;elPHu+="$wF";break;fi;done;done;eval $(echo "${elPHu}" $*)
```

Spits out any command or script in `lisp`, `perl`, `python` and `bash`.
