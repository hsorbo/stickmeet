#!/usr/bin/env python3
__author__      = "Havard Sorbo"
__copyright__   = "Copyright 2021, Flip Vernooij, 2022, Havard Sorbo"
__license__     = "MIT"
#depends on pyserial
import serial
import argparse
from serial.tools import list_ports
import time
from datetime import datetime

def to_signed(byte) -> int:
    integer = int(byte.hex(), 16)
    is_negative = (integer >> 7) & 1 == 1
    if is_negative is False:
        return integer
    return integer - 256

def import_data(device, verbose=False):
    ser = serial.Serial(device, baudrate=9600, timeout=1, bytesize=8, stopbits=1, parity='N')
    ser.flushInput()
    ser.flushOutput()
    ser.write(b'C')
    time.sleep(.1)
    now = datetime.now()

    for d in [ "%y","%m", "%d","%H", "%M" ]:
        date_component = now.strftime(d)
        date_as_byte = chr(int(date_component)).encode()
        ser.write(date_as_byte)

    dump_file = []
    c = 0
    x = 0
    cycle_count = 10
    while True:
        if c > cycle_count:
            if verbose: print("imported %d bytes" % len(dump_file))
            ser.close()
            break
        c = c + 1
        time.sleep(0.1)
        if verbose: print(f'Searching for entries {c} tries left')
        while ser.in_waiting > 0:
            x = x + 1
            if verbose: print(f'Reading byte {x}')
            dump_file.append(to_signed(ser.read(1)))
            # reset the cycle count here, should allow us to use a even lower cycle count.
            c = 0
    if ser.is_open:
        ser.close()
    return dump_file

def detect():
    for port in list_ports.comports():
        if "cu.usbmodem" in port.device:
            return port.device
    return ""

def format_dmp(raw_data): return ';'.join(str(e) for e in raw_data)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--tty", help="tty of the device to read from (e.g. /dev/ttyUSB0)")
    parser.add_argument("--output", help="output file to write to")
    args = parser.parse_args()
    tty = args.tty if args.tty else detect()
    verbose = True if args.output else False
    if not tty:
        print("No tty specified/detected")
        exit(1)
    print(tty)
    dmp = format_dmp(import_data(tty, verbose))
    if args.output:
        with open(args.output, 'wb') as f:
            f.write(dmp.encode())
    else:
        print(dmp)
    exit(0)