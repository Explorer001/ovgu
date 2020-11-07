# name of your application
APPLICATION = rpl_routing

# If no BOARD is found in the environment, use this default:
BOARD ?= nucleo-f767zi

# This has to be the absolute path to the RIOT base directory:
RIOTBASE ?= $(CURDIR)/../RIOT

# Include packages that pull up and auto-init the link layer.
# NOTE: 6LoWPAN will be included if IEEE802.15.4 devices are present
USEMODULE += auto_init_gnrc_netif
# add RPL module
USEMODULE += gnrc_rpl
# Activate ICMPv6 error messages
USEMODULE += gnrc_icmpv6_error
# Specify the mandatory networking modules for IPv6 and UDP
#USEMODULE += gnrc_ipv6_router_default
USEMODULE += gnrc_udp
# Additional networking modules that can be dropped if not needed
USEMODULE += gnrc_icmpv6_echo
#USEMODULE += gnrc_pktdump
USEMODULE += gnrc_ipv6_default
USEMODULE += gnrc_udp
USEMODULE += gnrc_sock_udp
#USEMODULE += posix_sockets
# Add also the shell, some shell commands
USEMODULE += shell
USEMODULE += shell_commands
USEMODULE += ps
USEMODULE += netstats_l2
USEMODULE += netstats_ipv6
USEMODULE += saul_default
USEMODULE += dht
USEMODULE += cc1101

# Comment this out to disable code in RIOT that does safety checking
# which is not needed in a production environment but helps in the
# development process:
DEVELHELP ?= 1

# Change this to 0 show compiler invocation lines by default:
QUIET ?= 1

include $(RIOTBASE)/Makefile.include

# Set a custom channel if needed
include $(RIOTMAKE)/default-radio-settings.inc.mk

#CFLAGS for prototype cc1101
CFLAGS += '-DDHT_PARAM_PIN=GPIO_PIN(PORT_D,3)'
CFLAGS += '-DCC110X_PARAM_CS=GPIO_PIN(PORT_D,14)'
CFLAGS += '-DCC110X_PARAM_GDO0=GPIO_PIN(PORT_D,15)'
CFLAGS += '-DCC110X_PARAM_GDO2=GPIO_PIN(PORT_F,12)'
CFLAGS += '-DCC110X_PARAM_PATABLE=(&cc110x_patable_433mhz)'
CFLAGS += '-DCC110X_PARAM_CONFIG=(&cc110x_config_433mhz_38kbps_50khz)'
CFLAGS += '-DCC110X_PARAM_CHANNELS=(&cc110x_chanmap_433mhz_50khz)'

#CFLAGS for miot cc1101
#CFLAGS += '-DCC110X_PARAM_SPI=SPI_DEV(0)'
#CFLAGS += '-DCC110X_PARAM_SPI_CLOCK=SPI_CLK_5MHZ'
#CFLAGS += '-DCC110X_PARAM_CS=GPIO_PIN(PORT_D, 15)'
#CFLAGS += '-DCC110X_PARAM_GDO0=GPIO_PIN(PORT_A, 4)'
#CFLAGS += '-DCC110X_PARAM_GDO2=GPIO_PIN(PORT_D, 14)'
#CFLAGS += '-DCC110X_PARAM_L2ADDR=CC110X_L2ADDR_AUTO'
#CFLAGS += '-DCC110X_PARAM_PATABLE=(&cc110x_patable_433mhz)'
#CFLAGS += '-DCC110X_PARAM_CONFIG=(&cc110x_config_433mhz_38kbps_50khz)'
#CFLAGS += '-DCC110X_PARAM_CHANNELS=(&cc110x_chanmap_433mhz_50khz)'

# rpl settings
CFLAGS += -DCONFIG_GNRC_IPV6_NIB_NUMOF=50
CFLAGS += -DCONFIG_GNRC_IPV6_NIB_OFFL_NUMOF=50

CFLAGS += '-DDEBUG_ASSERT_VERBOSE'
