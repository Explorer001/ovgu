#!/usr/bin/python2
import serial
from time import sleep

#
# destination address (IPv6) for measurement packets. set this to
# nodes IPv6 that packets are sent to
#
MEASURE_NODE_DEST_L3ADDR = "2001:db8::1"

#
# Layer 2 address of node that sends measurement packets.
#
MEASURE_NODE_L2ADDR = 0x79

#
# Layer 2 address of the dodag root.
#
ROOT_NODE_L2ADDR = 0x71

#
# number of measurement packets that are sent from source to dest node
#
MEASURE_NPACKETS = 100

#
# payload sent by measurement command.
#
MEASURE_PAYLOAD = "foobar"

#
# path of serial interface
#
SERIAL_PATH = "/dev/ttyACM0"

#
# baudrate used by serial
#
SERIAL_BAUD = 115200

#
# sends command over serial
# converts cmd to bytes and terminates command by carriage return
# @ser: serial command is sent over
# @cmd: command to be sent
def send_cmd(ser, cmd):
    _cmd = (cmd + "\r").encode("utf-8")
    ser.write(_cmd)

#
# receives whole line and decode it to utf8
# blocks if no line was received
# @ser: serial line is read from
# @return: received line
def recv_line(ser):
    line = ser.readline()
    
    try:
        line = line.decode("utf-8").strip("> ")
        return line
    except:
        return "encoding error"

#
# blocks until node is initalized
# when node send BATSIGNAL the node is booted
# @ser: serial node is connected
# @return: returns layer 2 address of node (indicated in BATSIGNAL)
#          and network interface address.
def wait_for_sync(ser):
    
    synced = False
    node_addr = 0

    while not synced:

        line = recv_line(ser)

        if "BATSIGNAL" not in line:
            continue

        synced = True
        line = line.strip()
        print(line)
        _signal, _addr, _netif  = line.split()
        node_addr = int(_addr, 16)
        netif = int(_netif, 10)

    return (node_addr, netif)

#
# starts measurement command on node
# @ser: serial connection to node
def start_measurement(ser):
    measure_cmd = "send %s %s %s" % (MEASURE_NODE_DEST_L3ADDR, MEASURE_NPACKETS, MEASURE_PAYLOAD)
    send_cmd(ser, measure_cmd)

#
# configures dodag root for operation.
# @ser: serial connection to node
# @netif: network interface number
def configure_root(ser, netif):
    ip_cmd = "ifconfig %s add %s" % (netif, MEASURE_NODE_DEST_L3ADDR)
    root_cmd = "rpl root 1 %s" % MEASURE_NODE_DEST_L3ADDR
    send_cmd(ser, ip_cmd)
    send_cmd(ser, root_cmd)
    send_cmd(ser, "listen")

#
# main measurement function. it restarts all nodes
# for nodes that do not match MEASURE_NODE_L2ADDR this function
# logs only the serial output.
# if node matches MEASURE_NODE_L2ADDR it kicks off measurement
#
def perform_measurement():
    ser = serial.Serial(SERIAL_PATH, SERIAL_BAUD)
    ser.flush()
    ser.flushInput()

    addr, netif = wait_for_sync(ser)
    if addr == ROOT_NODE_L2ADDR:
        configure_root(ser, netif)

    if addr == MEASURE_NODE_L2ADDR:
        sleep(10)
        start_measurement(ser)

    # save nodes serial output
    while True:
        line = recv_line(ser).strip()
        print(line)

perform_measurement()
