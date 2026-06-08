#!/usr/bin/env fish

function greet --argument-names name
    set -l message "Hello, $name"
    if test -n "$message"
        echo $message
    end
end

for user in world fresh
    greet $user
end
