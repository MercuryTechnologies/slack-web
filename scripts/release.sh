#!/usr/bin/env bash
set -xeu

cabal_files=(*.cabal)
if [[ ${#cabal_files[@]} != 1 ]]; then
    echo "Error: non-1 number of cabal_file"
    exit 1
fi
cabal_file=${cabal_files[0]}
package_name=${cabal_file//.cabal}

if [[ -e package.yaml ]]; then
    hpack --force
fi

version=$(perl -ne '/^version: (.+)$/ && print "$1\n"' "${cabal_file}" | tr -d ' ')
git fetch origin

# Ensure we are on origin/main
[[ $(git rev-parse origin/HEAD) == $(git rev-parse HEAD) ]] || (echo "not up to date"; exit 1)

# Verify Nix
nix flake check -Lv

cabal sdist

sdist="dist-newstyle/sdist/$package_name-${version}.tar.gz"
tar tf "$sdist"

echo -e "\n\nWould you like to release this sdist as $version? Enter the version number if so."
read resp

[[ "$resp" == "$version" ]]

cabal upload "$sdist"
git tag -a -m "Version ${version}" "v${version}"
git push origin "v${version}"
