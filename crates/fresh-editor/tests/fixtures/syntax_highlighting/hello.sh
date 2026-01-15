#!/bin/bash

function greet() {
    echo "Hello, $1!"
}

MESSAGE="World"
greet "$MESSAGE"
