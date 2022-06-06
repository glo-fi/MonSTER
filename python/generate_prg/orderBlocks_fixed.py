from androguard.misc import AnalyzeAPK
from androguard.decompiler.dad.decompile import DvMethod
from androguard.core.analysis.analysis import DVMBasicBlock
import os
import random
import string
import io
from pathlib import Path
from sys import getsizeof
import time

dirname = os.path.dirname(__file__)
api_candidates = ["Landroid/", "Ldagger/" "Lcom/android/internal/util", "Ldalvik/", "Ljava/", "Ljavax/", "Lorg/apache/", "Lorg/json/", "Lorg/w3c/dom/", "Lorg/xml/sax", "Lorg/xmlpull/v1/", "Ljunit/", "Landroidx/", "Lorg/bouncycastle/", "Lkotlinx/", "Lorg/joda/", "Landroid/support/", "Lorg/intellij/", "Lcom/google/common/", "Lcom/google/android/material/", "Lcom/google/protobuf", "Lcom/google/zxing/", "Lcom/google/android/gms", "Lcom/microsoft/", "Lcom/android/", "Lorg/threeten/", "Lokio/", "Lio/jsonwebtoken/", "Lcom/tinder/", "Lcom/airbnb/", "Lcom/koin/", "Lorg/koin/", "Lcoil/", "Lcom/itextpdf/", "Lcom/squareup/", "Lcom/github/", "Lcom/sun/", "Lcom/facebook/", "Lcom/lugg/", "Lcom/ocetnik/", "Lretrofit2/", "Lio/realm/io_realm_sync_", "Lcom/google/firebase/", "Lcom/bumptech/", "Lorg/conscrypt/", "Lcom/pedrouid/", "Lcom/reactnativecommunity/", "Lj$/", "Lcom/rnfs/", "Lcom/google/android/play/", "Lcom/rnziparchive/", "Lio/realm/", "Lcom/swmansion/", "Lcom/th3rdwave/", "Lcom/transistorsoft/", "Lorg/spongycastle/", "Lio/realm/", "Lorg/mozilla/", "Lcom/uphyca/", "Lcom/upokecenter/", "Ldgca/verifier/", "Lnet/lingala/", "Lcom/fasterxml/", "Lio/reactivex/", "Lkotlin/", "Lorg/jetbrains/", "Lcom/google/", "Lokhttp3/", "Lcom/crashlytics/", "Lsun/misc/", "C>", "S>", "B>", "I>", "J>", "Z>", "F>", "D>"]
    
class Atm():
    def __init__(self, method_name):
        self._method_name = method_name

    def __eq__(self, other):
        """Overrides the default implementation"""
        if isinstance(other, Atm):
            return self._method_name == other.method_name
        return False

    @property
    def method_name(self):
        return self._method_name

    def __repr__(self):
        return f"&{self._method_name}"

    def expr_print(self):
        return f"Atm {self._method_name}"

    def buf_print(self, buf, filename):
        buf.write(str(self))
        print(buf.getvalue(), file=filename, end = " ")
        buf.truncate(0)
        buf.seek(0)

class Cal():
    def __init__(self, method_name):
        self._method_name = method_name

    def __eq__(self, other):
        """Overrides the default implementation"""
        if isinstance(other, Cal):
            return self._method_name == other.method_name
        return False

    @property
    def method_name(self):
        return self._method_name

    def __repr__(self):
        return f"{self._method_name}"

    def expr_print(self):
        return f"Cal {self._method_name}"

    def buf_print(self, buf, filename):
        buf.write(str(self))
        print(buf.getvalue(), file=filename, end = " ")
        buf.truncate(0)
        buf.seek(0)

class Seq():
    def __init__(self, expr1, expr2):
        self._expr1 = expr1
        self._expr2 = expr2

    @property
    def expr1(self):
        return self._expr1

    @property
    def expr2(self):
        return self._expr2
  
    @classmethod
    def fromList(cls, seq_list : list):
        '''Class method to turn a list into a series of Seq.
           For instance, ["a", "b", "c"] -> Seq("a", Seq("b", "c"))'''
        if len(seq_list) == 1:
            return seq_list[0]
        head, *tail = seq_list
        expr1 = head
        expr2 = cls.fromList(tail)
        return cls(expr1, expr2)

    def __repr__(self):
        return f"( {self._expr1} ; {self._expr2} )"

    def expr_print(self):
        return f"Seq (%s) (%s)" % (self._expr1.expr_print(), self._expr2.expr_print())

    def filePrint(self, filename):
        with open(filename + '.txt', mode='a+') as file_object:
            print(self, file=file_object)

    def buf_print(self, buf, filename):
        buf.write("( ")
        self._expr1.buf_print(buf, filename)
        buf.write(" ; ")
        self._expr2.buf_print(buf, filename)
        buf.write(") ")
        print(buf.getvalue(), file=filename, end = " ")
        buf.truncate(0)
        buf.seek(0)


class Cnd():
    def __init__(self, expr1, expr2):
        self._expr1 = expr1
        self._expr2 = expr2

    @property
    def expr1(self):
        return self._expr1

    @property
    def expr2(self):
        return self._expr2

    @classmethod
    def fromList(cls, cnd_list : list):
        '''Class method to turn a list into a series of Cnd.
           For instance, ["a", "b", "c"] -> Cnd("a", Cnd("b", "c"))'''
        if len(cnd_list) == 1:
            return cnd_list[0]
        head, *tail = cnd_list
        expr1 = head
        expr2 = cls.fromList(tail)
        return cls(expr1, expr2)
  
    def filePrint(self, filename):
        with open(filename + '.txt', mode='a+') as file_object:
            print(self, file=file_object)

    def __repr__(self):
        return f"( {self._expr1} ? {self._expr2} )"

    def expr_print(self):
        return f"Cnd (%s) (%s)" % (self._expr1.expr_print(), self._expr2.expr_print())

    def buf_print(self, buf, filename):
        buf.write("( ")
        self._expr1.buf_print(buf, filename)
        buf.write(" ? ")
        self._expr2.buf_print(buf, filename)
        buf.write(" )")
        print(buf.getvalue(), file=filename, end = " ")
        buf.truncate(0)
        buf.seek(0)

class BBlock():
    def __init__(self, block: DVMBasicBlock, name: str):
        self._is_empty = False
        self._block = block
        self._block_name = self.generate_name(name)
        self._children = self.generate_children(name)
        self._instruction_list = self.generate_instructions()
        self._block_expression = self.instructions_to_expression()
        self._children_expression = None
        self._complete_expression = None
        

    @property
    def children(self):
        return self._children

    @children.setter
    def children(self, value):
        self._children = value

    @property
    def block(self):
        return self._block

    @block.setter
    def block(self, value):
        self._block = value

    @property
    def block_expression(self):
        return self._block_expression

    @block_expression.setter
    def block_expression(self, value):
        self._block_expression = value

    @property
    def children_expression(self):
        return self._children_expression

    @children_expression.setter
    def children_expression(self, value):
        self._children_expression = value

    @property
    def complete_expression(self):
        return self._complete_expression

    @complete_expression.setter
    def complete_expression(self, value):
        self._complete_expression = value

    @property
    def block_name(self):
        return self._block_name

    @block_name.setter
    def block_name(self, value):
        self._block_name = value

    @property
    def is_empty(self):
        return self._is_empty

    @is_empty.setter
    def is_empty(self, value):
        self._is_empty = value

    def generate_name(self, name):
        offset = self._block.get_start()
        if offset == 0:
            return name
        else:
            return name + "_" + str(offset)

    def generate_children(self, name):
        child_list = self._block.get_next()
        name_list = []
        if (len(child_list) == 0):
           return []
        else:
           for child in child_list:
               if child[2].get_start() == 0:
                    name_list.append(name)
               else:
                    name_list.append(name + "_" + str(child[2].get_start()))                    
           return list(set(name_list))

    def generate_instructions(self):
        ''' Given a block, produce a list of the method calls in that block.
            Return: List of instructions'''
        temp_instrucs = [] 
        for instruc in self._block.get_instructions():
             atm_bool = False
             op_value = instruc.get_op_value()
             if ((0x6e <= op_value <= 0x72) or (0x74 <= op_value <= 0x78)):
                 x = str(instruc.get_output()).split(',')[-1]
                 y = x.replace(";", ">").replace("-", "").replace(" ", "").replace(">>", ">")     # remove ;'s and -'s
                 while y[0] == '[':
                     y = y[1:]
                 for candidate in api_candidates:
                     if y.startswith(candidate):
                         atm_bool = True
                         break
                 if atm_bool:
                     temp_instrucs.append(Atm(y))
                 else:
                     temp_instrucs.append(Cal(y))
             elif (0xe <= op_value <= 0x11):
                 temp_instrucs.append(Atm("RETURN"))
        return temp_instrucs

    def instructions_to_expression(self):
        '''Convert a list of instructions into a sequence of instructions.'''
        length = len(self._instruction_list)
        if length == 0:
            self._is_empty = True
            return Atm("nop")
        elif length == 1:
            return self._instruction_list[0]
        else:
            return Seq.fromList(self._instruction_list)

    def children_to_expression(self):
        child_list = []
        cal_list = []
        if len(self._children) == 0:
            self._children_expression = None
        elif len(self._children) == 1:
            self._children_expression = Cal(self._children[0])
        else:
            for child in self._children:
                cal_list.append(Cal(child))
            self._children_expression = Cnd.fromList(cal_list)

    def generate_complete_expression(self):
        if self._children_expression == None:
            self._complete_expression = self._block_expression
        else:
            self._complete_expression = Seq(self._block_expression, self._children_expression)

    @property
    def block_name(self):
        return self._block_name

    @block_name.setter
    def block_name(self, value):
        self._block_name = value


class APK():
    def __init__(self, filepath):
        self.a, self.d, self.dx = AnalyzeAPK(filepath)
        self.block_dict = {}
        self.simplified_block_dict = {}

    def wipeDict(self):
        self.block_dict.clear()

    def simplify_block_dict(self):
        for block in self.block_dict:
            if self.block_dict[block].is_empty and self.block_dict[block].block.get_start() != 0:
                continue
            else:
                previous_children = []
                visited_children = []
                if len(self.block_dict[block].children) != 0:
                    while previous_children != self.block_dict[block].children:
                        previous_children = list(self.block_dict[block].children) 
                        for child in self.block_dict[block].children:
                            if self.block_dict[child].is_empty:
                                for grandchild in self.block_dict[child].children:
                                    if grandchild not in self.block_dict[block].children and grandchild not in visited_children:
                                        self.block_dict[block].children.append(grandchild)
                                        visited_children.append(grandchild)
                                self.block_dict[block].children.remove(child)
                self.simplified_block_dict[self.block_dict[block].block_name] = self.block_dict[block]

    def get_ordered_methods_in_class(self, outer_class_name, filename, first):
        '''
        Orders the basic blocks of all methods in a class according to the orderBlocks() algorithm above.
        '''
        method_list = []
        for meth in self.dx.classes[outer_class_name].get_methods():
            if meth == None:
                continue
            full_name = str(meth.full_name)
            edited_name = full_name.replace(";", ">").replace(" ", "").replace("-", "") # remove ;'s and " "
            start_block = next(meth.get_basic_blocks().get(), None) 
            if start_block == None: #This is equivalent to checking if a method is external
                instrucs = Atm("nop")
                with io.StringIO() as buf:
                     if not first:
                         buf.write(" - \n")
                     else:
                         first = False
                     buf.write(edited_name + " := ")
                     instrucs.buf_print(buf, filename)
                     print(buf.getvalue(), file=filename, end = " ")
                     buf.truncate(0)
                     buf.seek(0)
            else:
                for block in meth.get_basic_blocks():
                    bblock = BBlock(block, edited_name)
                    self.block_dict[bblock.block_name] = bblock 
                self.simplify_block_dict()
                for bblock in self.simplified_block_dict:
                    self.simplified_block_dict[bblock].children_to_expression()
                    self.simplified_block_dict[bblock].generate_complete_expression()
                    with io.StringIO() as buf:
                        if not first:
                            buf.write(" - \n")
                        else:
                            first = False
                        buf.write(self.simplified_block_dict[bblock].block_name + " := ")
                        self.simplified_block_dict[bblock].complete_expression.buf_print(buf, filename)
                        print(buf.getvalue(), file=filename, end = " ")
                        buf.truncate(0)
                        buf.seek(0)
            self.block_dict.clear()
            self.simplified_block_dict.clear()


    def get_ordered_methods_in_APK(self, filename = "disass"): 
        '''
        Calls get_ordered_methods_in_class() on every non-api/external class in an APK

        '''
        full_filename = filename + "_apk" + ".output"
        f = open(full_filename, 'a+')
#        f.write("{ ")
        first = True
        for classes in self.dx.find_classes():
            api = False
            if classes.is_android_api():
                continue
            if classes.is_external():
                continue
            for candidate in api_candidates:
                if str(classes.name).startswith(str(candidate)):
                    api = True
                    continue 
            if api:
                continue
            else:
                self.get_ordered_methods_in_class(classes.name, f, first)
                first = False
#        f.write(" }")
        f.close()
                




