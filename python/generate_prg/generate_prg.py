#!/usr/bin/python3
import argparse
from orderBlocks_fixed import APK

bool_exceptions = False

parser = argparse.ArgumentParser(description='Generate CallFlow Prg for APK.')
parser.add_argument("-x", "--exceptions", help="include exception analysis in callflow prg",
                     action="store_true")
parser.add_argument("-f", "--folder", help="output folder",
                     action="store")
parser.add_argument("-m", "--memory", help="memory limit for individiual methods (Mb)",
                     action="store")
parser.add_argument("-t", "--time", help="time limit for processing methods (seconds)",
                     action="store")
parser.add_argument("APK")
args = parser.parse_args()

if args.exceptions:
    bool_exceptions = True

apk_filename = args.APK
apk = APK(apk_filename)

apk.get_ordered_methods_in_APK(args.folder)
