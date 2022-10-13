#!/usr/bin/env jq -Mf

def to_set:
    map({key: ., value: true}) | from_entries
    ;

def sanitize($set):
    . as $inp |
    if $set[$inp.key] == true then
        {key: $inp.key, value: "aaaa"}
    else
        $inp
    end
    ;

def excludes: ["token", "event_context"] | to_set;

. | to_entries | map(sanitize(excludes)) | from_entries
