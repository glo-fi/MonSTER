import argparse
from convert import genericReplaceSave

bool_exceptions = False

parser = argparse.ArgumentParser(description='Replace Methods with Keyword.')
parser.add_argument("-i", "--input", help="input file",
                     action="store")
parser.add_argument("-s", "--source", help="source file for methods.",
                     action="store")
parser.add_argument("-w", "--word", help="word to replace",
                     action="store")
args = parser.parse_args()


genericReplaceSave(args.input, args.source, args.word)
