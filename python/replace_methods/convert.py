#!/usr/bin/env python3

import re
import sys

def openfile(filename):
    F = open(filename, 'r')
    linelist = F.readlines()
    F.close()
    F = open("edited_" + filename, 'a')
    for line in linelist:
        line_array = line.split(" ")
        towrite = "%s\n" % (filepath_convert(line_array[0]) + method_convert(line_array[2]).replace(")>", ">)") + return_convert(line_array[1]))
        F.write(towrite)
        #print(towrite)
    F.close()

def filepath_convert(filepath):
    filepath = 'L' + filepath[1:]
    filepath = filepath[:-1] + '>'
    filepath = filepath.replace(".", "/")
    return filepath

def method_convert(method):
    methodAndArguments = method.split("(")
    method_name = methodAndArguments[0]
    arguments = methodAndArguments[1][:-1]
    argument_array = arguments.split(",")
    for idx, argument in enumerate(argument_array):
        argument = "L" + argument
        if "[]" in argument:
            num_arrays = argument.split("[")
            argument_type = num_arrays[0]
            for number in range(len(num_arrays) - 1):
                argument_type = "[" + argument_type
            argument = argument_type
        argument = argument.replace(".", "/")
        argument = argument.replace("Ldouble", "D")
        argument = argument.replace("Lint", "I")
        argument = argument.replace("Lchar", "C")
        argument = argument.replace("Lboolean", "Z")
        argument = argument.replace("Lbyte", "B")
        argument = argument.replace("Lshort", "S")
        argument = argument.replace("Lfloat", "F")
        argument = argument.replace("Llong", "J")
        if "/" in argument:
            argument_array[idx] = argument + ">"
        else:
            argument_array[idx] = argument
    arguments = "".join(argument_array)
    method = method_name + "(" + arguments
    return method
    
def return_convert(ret):
    ret = ret.replace("double", "D")
    ret = ret.replace("void", "V")
    ret = ret.replace("int", "I")
    ret = ret.replace("char", "C")
    ret = ret.replace("boolean",  "Z")
    ret = ret.replace("byte", "B")
    ret = ret.replace("short", "S")
    ret = ret.replace("float", "F")
    ret = ret.replace("long", "J")
    ret = ret.replace(".", "/")
    if "/" in ret:
        ret = "L" + ret + '>'
    else:
        ret
    if "[]" in ret:
        num_arrays = ret.split("[")
        ret_type = num_arrays[0]
        for number in range(len(num_arrays) - 1):
            ret_type = "*" + ret_type
        ret = ret_type
    return ret
       

def generic_replace(prg, filename, word):
    source = open(filename, "r")
    sourcelist = source.readlines()
    source.close()
    for line in sourcelist:
        if (line.strip('\r\n') in prg):
            prg = prg.replace(line.strip('\r\n'), word)
    return prg

def genericReplaceSave(filename, source_file, word):
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    g = open("edited_" + filename, 'a+')
    for line in lines:
        string = generic_replace(line, source_file, word)
        g.write(string)
    g.close()    






