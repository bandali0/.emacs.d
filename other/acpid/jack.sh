#!/bin/bash
# acpi script that takes an entry for headphone actions

case "$1" in
    jack/headphone)
        case "$3" in
            plug)
                echo "⮜" > /tmp/spkicon
                ;;
            unplug)
                echo "⮟" > /tmp/spkicon
                ;;
    esac
    ;;
esac

# vim:set ts=4 sw=4 ft=sh et:
