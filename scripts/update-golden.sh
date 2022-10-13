#!/usr/bin/env bash
set -eu

if [[ $# != 1 ]]; then
    echo "usage: $0 DIR" >&2
    echo "Updates the golden files in the specified directory" >&2
    exit 1
fi

confirm() {
    echo "$1"
    echo -n "[Y/n] "
    read confirmation
    case "$confirmation" in
        y | Y | yes | "")
            return 0 ;;
        *)
            return 1 ;;
    esac
}

promptUpdate() {
    echo "diff: "
    # for god knows why, this fails
    diff -u $2 $1 || true

    if confirm "update ${2}?"; then
        cp $1 $2
    else
        echo "skipping ${2}"
    fi
}

dir="$1"
for f in ${dir}/*.golden; do
    baseName="${f%%.golden}"
    actualName="${baseName}.actual"
    if cmp -s "$actualName" "$f"; then
        echo "$f up to date"
    else
        promptUpdate "$actualName" "$f"
    fi
done

