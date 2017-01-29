#!/bin/bash

# Abort if any of the commands fail.
set -o errexit

docs_branch=gh-pages

previous_branch=`git rev-parse --abbrev-ref HEAD`
commit_hash=`git log -n 1 --format="%H" HEAD`

if [ "$previous_branch" != "master" ]; then
    echo "$0 must be run from branch 'master'"
    exit 1
fi

stack haddock --no-haddock-deps

# Switch to deployment branch.
git checkout ${docs_branch}

# Copy generated files to root directory.
rsync -av .stack-work/install/x86_64-linux/lts-7.14/8.0.1/doc/* haddock/

# Add and commit changes.
git add --all haddock/
git commit -m "Push docs from ${commit_hash}"
git push

# Switch back to previous branch.
git checkout ${previous_branch}
