#!/usr/bin/env python3
import sys
import struct
import datetime
import json

HEADER_LEN=10
SHOT_LEN=16
DIRECTION=["IN","OUT"]
SHOT_TYPE=["CSA","CSB","STD","EOC"]

def to_unsigned(x): return x if x > -1 else 256 + x

def to_binary(semicolon_separated):
    string_bytes = [l for l in semicolon_separated.split(';') if l.strip()]
    return bytearray(map(to_unsigned,map(int,string_bytes)))

def parse_survey_header(data):
    (hdr,year,month,day,hh,mm,name,direction) = struct.unpack("bbbbbb3sb",data)
    if hdr != 2: raise Exception("Couldn't find magic number")
    if year not in range(16,24): raise Exception("Year is out of range")
    return {
        "date": datetime.datetime(2000+year, month, day, hh, mm).isoformat(),
        "name": name.decode("utf-8"),
        "direction":DIRECTION[direction]}

def parse_survey_shot(data):
    s = struct.unpack(">bhhhhhhhb",data)
    return {
        "type": SHOT_TYPE[s[0]],
        "head_in": s[1]/10,
        "head_out": s[2]/10,
        "length": s[3]/100,
        "depth_in": s[4]/100,
        "depth_out": s[5]/100,
        "pitch_in": s[6]/10,
        "pitch_out": s[7]/10,
        "marker": s[8] }


def decode(data):
    x = 0
    surveys = []
    while True:
        hdr_data = data[x:x+HEADER_LEN]
        if len(hdr_data) < HEADER_LEN: break
        survey=parse_survey_header(hdr_data)
        x+=HEADER_LEN
        shots = []
        for i in range(x,10000,SHOT_LEN):
            shot_arr = data[i:i+SHOT_LEN]
            if len(shot_arr) < SHOT_LEN: break
            shot = parse_survey_shot(shot_arr)
            shots.append(shot)
            x += SHOT_LEN
            if shot["type"] == "EOC": break
        survey["shots"] = shots
        surveys.append(survey)
    return surveys

if __name__ == "__main__":
    filename = sys.argv[1]
    data = decode(to_binary(open(filename).read()))
    print(json.dumps(data, sort_keys=True, indent=2))
