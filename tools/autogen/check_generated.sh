#!/bin/bash
#
# Verify that the committed generated sources match their templates.
#
# The generated .f90 under src/core/generated/ is committed so that building fairport needs
# neither fypp nor Python. The cost of that is exactly one failure mode: a
# template edited without rerunning autogen.sh. This check closes it, and CI
# runs it on every push.

set -uo pipefail
cd "$(dirname "$0")"

if ! command -v fypp > /dev/null 2>&1; then
  echo "check_generated: fypp is not installed; skipping"
  echo "  install it with: python3 -m pip install fypp"
  exit 0
fi

status=0
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT

# Parse the generate/copy lines of autogen.sh, so that adding a module there is
# picked up here automatically.
while read -r template output; do
  destination=$(grep -E "^cp $output " autogen.sh | awk '{print $3}')
  [ -n "$destination" ] || continue

  fypp "$template" > "$scratch/$output" 2>/dev/null
  if ! diff -q "$scratch/$output" "$destination$output" > /dev/null 2>&1; then
    echo "OUT OF DATE: $destination$output does not match $template"
    diff -u "$destination$output" "$scratch/$output" | head -40
    status=1
  fi
done < <(grep -E '^fypp .*> ' autogen.sh | awk '{print $2, $4}')

if [ "$status" -eq 0 ]; then
  echo "generated sources: up to date"
else
  echo ""
  echo "Run tools/autogen/autogen.sh and commit the result."
fi
exit "$status"
