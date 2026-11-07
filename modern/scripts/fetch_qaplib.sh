#!/usr/bin/env bash
# Downloads QAPLIB instances and their best known solutions (NAME.dat, NAME.sln).
#   scripts/fetch_qaplib.sh DIRECTORY NAME...
# Files already in DIRECTORY are kept.
set -euo pipefail

if [ $# -lt 2 ]; then
  echo "usage: $0 DIRECTORY NAME..." >&2
  exit 2
fi
directory=$1
shift
qaplib=https://coral.ise.lehigh.edu/wp-content/uploads/2014/07
mkdir -p "$directory"

# the server drops connections now and then: retry, and never keep a partial file
fetch() {
  local url=$1 file=$2
  [ -s "$file" ] && return
  curl -fsS --retry 5 --retry-all-errors --connect-timeout 20 --max-time 120 -o "$file.tmp" "$url"
  mv "$file.tmp" "$file"
}

for name in "$@"; do
  fetch "$qaplib/data.d/$name.dat" "$directory/$name.dat"
  fetch "$qaplib/soln.d/$name.sln" "$directory/$name.sln"
done
echo "$# instances in $directory"
