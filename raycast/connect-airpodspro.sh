#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Connect AirPods Pro
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🎧

# Documentation:
# @raycast.description Connect Airpods Pro
# @raycast.author tosaka07
# @raycast.authorURL https://github.com/tosaka07

# Main:

if ! type "BluetoothConnector" > /dev/null 2>&1; then
  echo "BluetoothConnector is not installed."
  exit 1
fi

if ! type "SwitchAudioSource" > /dev/null 2>&1; then
  echo "SwitchAudioSource is not installed."
  exit 1
fi

AIR_PODS_ADDRESS=a4-c6-f0-bb-1e-17
AIR_PODS_NAME="翔悟のAirPods Pro"

# 通知バナーが不要なら `--notify` は省いて良い
BluetoothConnector --connect $AIR_PODS_ADDRESS --notify
for ((i=0 ; i<10 ; i++))
do
    if [ "Connected" = $(BluetoothConnector -s $AIR_PODS_ADDRESS) ]; then
        sleep 1
        if [[ $(SwitchAudioSource -s  $AIR_PODS_NAME) =~ "^Could not" ]]; then
            osascript -e 'display notification "Failed to switch audio source(´・︵・｀)"'
            break
        fi
        sleep 1
        # 前回の設定に関わらず、とりあえず10%くらいがちょうど良く感じる
        osascript -e "set Volume 1"
        say -v samantha Connected
        break
    fi
    sleep 1
done
