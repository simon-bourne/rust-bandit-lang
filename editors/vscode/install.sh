#!/usr/bin/env bash

exec rsync -av --delete --exclude .git "$(dirname "$0")/" "$HOME/.vscode-oss/extensions/bandit-vscode"
