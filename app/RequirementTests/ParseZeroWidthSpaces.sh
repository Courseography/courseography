#!/usr/bin/env bash

SOURCE_DIR="$(pwd | grep -oE "^.+courseography")/app"
TEST_FILE="${SOURCE_DIR}/RequirementTests/zeroWidthSpacesTests"

cd "$(mktemp -d)" || exit 2
echo "Zero-width space test suite:"

# Compile requirement parser
ghc -i"$SOURCE_DIR" -dynamic -outputdir . -o "./reqparser" "${SOURCE_DIR}/RequirementTests/ReqParserMain.hs" 1>/dev/null

# Compare inputs and outputs
num_tested=0
num_failed=0
exit_code=0

# Save the trimmed tests to a file instead of piping to the while loop because
# the latter creates a subshell, where num_tested/failed can't be mutated
egrep '^[^#]' "$TEST_FILE" > ./trimmed_tests

while read input
do
  actual="$(echo "$input" | ./reqparser)"
  read expected

  if [[ "$actual" != "$expected" ]]; then
    ((num_failed++))
    exit_code=1
    cat <<EOF

For input "$input":
    Expected: $expected
         Got: $actual
EOF
  fi

  ((num_tested++))
done < ./trimmed_tests

printf 'Tested: %s  Failed: %s\n' "$num_tested" "$num_failed"
exit $exit_code
