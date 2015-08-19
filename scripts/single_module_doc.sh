
module_file=$1
module_pattern='(\w+)\.erl$'
if [[ $module_file =~ $module_pattern ]]; then
  module_name="${BASH_REMATCH[1]}"
fi

general_pattern='^(%%.+|\w+\(.+|)$'
lines=`cat src/${module_file} | grep -P -e $general_pattern`$'\n'

doc_pattern='^%% @doc (.+)'
url_pattern='^%% (http.+)'
func_pattern='^((\w+)\([^\(]+\))'
empty_pattern='^$'

current_doc=""
current_url=""
current_names=""
current_funcs=""

echo "# ${module_name}"

while read -r line; do
  if [[ $line =~ $doc_pattern ]]; then
    current_doc="${BASH_REMATCH[1]}"
    current_url=""
    current_funcs=""
    current_names=""
  elif [[ $line =~ $url_pattern ]]; then
    current_url="${BASH_REMATCH[1]}"
  elif [[ $line =~ $func_pattern ]]; then
    if [[ -z $current_names ]]; then
      current_funcs="${BASH_REMATCH[1]}"
      current_names="${BASH_REMATCH[2]}"
    else
      current_funcs="${current_funcs}"$'\n'"${BASH_REMATCH[1]}"
      current_names="${current_names} ${BASH_REMATCH[2]}"
    fi
  elif [[ $line =~ $empty_pattern ]]; then
    if [[ -n $current_names ]] && [[ -n $current_url ]]; then
      # Remove non-unique names and separate by comma+space
      current_names=$(for i in `echo $current_names`; do
          echo $i
      done | sort | uniq | tr '\n' ',' | sed 's/,$//' | sed 's/,/, /')

      # Substitute variable names for binary strings hints where possible
      current_funcs=$(echo "${current_funcs}" | sed 's/Device/<<"DEVICE-ID">>/')
      current_funcs=$(echo "${current_funcs}" | sed 's/Dist/<<"DISTRIBUTION-ID">>/')
      current_funcs=$(echo "${current_funcs}" | sed 's/Key/<<"KEY-ID">>/')
      current_funcs=$(echo "${current_funcs}" | sed 's/Stream/<<"STREAM-NAME">>/')

      echo
      echo "### $current_names"
      echo "${current_doc}"
      echo "> [${current_url}](${current_url})"
      echo
      echo '```erlang'
      while read -r func; do
        echo "${module_name}:${func}."
      done <<< "$current_funcs"
      echo '```'
    fi
  fi
done <<< "$lines"
