#!/usr/bin/env bash

rsync -av --delete --exclude .git "$(dirname "$0")/" "$HOME/.vscode/extensions/bandit-vscode"
