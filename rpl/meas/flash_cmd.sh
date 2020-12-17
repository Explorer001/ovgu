#!/bin/bash

IMAGE_FILE=$1

sh -c "openocd \
      -c 'source [find interface/stlink.cfg]' \
      -c 'transport select hla_swd' \
      -c 'source [find target/stm32f7x.cfg]' \
      -c 'reset_config connect_assert_srst' \
      -c 'init' \
      -c 'targets' \
      -c 'reset halt' \
      -c 'flash write_image erase \"${IMAGE_FILE}\"' \
      -c 'verify_image \"${IMAGE_FILE}\"' \
      -c 'reset run' \
      -c 'shutdown'"
