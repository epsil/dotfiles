#!/bin/sh
# Use a less aggressive hard disk power management to get rid of
# clicking noise when the drive is parking its heads
hdparm -B 255 /dev/sda
