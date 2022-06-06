## Outline of methodology to use callflow.hs as a static analysis tool

Ultimately, callflow.hs can be used to determine the order in which a list of specified methods is called, regardless of mutual recursion. Therefore, if a property can be expressed as a list of methods in the source code of the app in a certain order, it is a strong contender for a property that callflow.hs can detect.

# Step 1. Determine the property you hope to measure

Preferably, we want to choose a property that is present in some apps, but not present in others however this is not vital. To figure out what properties may be worthwhile, we need to do some manual code analysis. As a rough example, imagine that in some of our apps _method1()_ is called before _method2()_ but in other apps only _method2()_ is called. Note that we can also check the order of classes of methods (for instance, we want to check if _methodA1()_ or _methodB1()_ occurs before _method2()_ but we don't care which)

# Step 2. Generate the PRG of the APK using the Python code in python/generate_prg

The syntax for generating the prg is 

```
python3 generate_prg.py -f [output name] [input_apk]
```

This will output a text file containing the PRG which can then be parsed by the Haskell code.

# Step 3. Set up the Monoid in the Haskell code

As far as I'm aware, there's no way to modify a data structure in Haskell after it has been defined, therefore we have to manually set up the Monoid before running anything else. The Monoid is defined on line 173 in megaparsec.hs (megaparsec.hs contains both the parser and callflow.hs in one combined file) and looks like:

```
data AMon = U | A | B | AB | BA deriving (Eq, Ord, Show)
instance Mon AMon where
  unit = U
  mult a  U  = a
  mult U  a  = a
  mult A  A  = A 
  mult B  B  = B
  mult BA A  = BA 
  mult B  _  = BA
  mult _  _  = AB
  lift "&A" = A
  lift "&B" = B 
  lift _   = U 
```

We need to manually define the multiplication rules as well as the **keywords** that will be used to determine the order of method invocation. In the example outlined above we may choose something like:

```
data AMon = U | METH1 | METH2 | METH1\_2 | METH2\_1 deriving (Eq, Ord, Show)
instance Mon AMon where
  unit = U
  mult a  U  = a
  mult U  a  = a
  mult METH1  METH1  = METH1 
  mult METH2  METH2  = METH2
  mult METH2\_1 METH1  = METH2\_1 
  mult METH2  _  = METH2\_1
  mult _  _  = METH1\_2
  lift "&METH1" = METH1
  lift "&METH2" = METH2 
  lift _   = U 
```

where "METH1" and "METH2" are our keywords. METH1\_2 indicates the situation where _method1()_ occurs before _method2()_ and vice versa for METH2\_1.

Note that the monoid can be extended to have more than 4 elements but the multiplcation rules need to be expanded.

# Step 4. Insert your chosen keywords into the PRG using the python code in python/replace_methods

In order to check the order of methods in the PRG, it is easiest if we replace those methods with a keyword (hence why we used keywords in step 3). _replace\_methods.py_ can be used to do this. The syntax is:
```
python3 replace\_methods.py -s [SOURCE\_FILE] -i [INPUT\_FILE] -w [KEYWORD]
```

Where [SOURCE_FILE] is a text file containing the list of methods you want to replace in [INPUT_FILE] with [KEYWORD].

Imagine in step 2, we have outputted a file called _NHS\_prg.output_, and we want to see if _method1()_ occurs before _method2()_ in the code. We have set up the Monoid in the manner described in step 3. We then create a textfile containing _method1()_, call it _METH1.txt_ and a seperate textfile containing _method2()_, call it _METH2.txt_. We call the above line of code twice:

```
python3 replace\_methods.py -s METH1.txt -i NHS\_prg.output -w METH1
python3 replace\_methods.py -s METH2.txt -i NHS\_prg.output -w METH2
```

This replaces any method listed in METH1.txt with the keyword "METH1" and similarly for METH2.txt. The output is stored in _edited\_NHS\_prg.output_.

For small lists of functions, you can also just use sed, if that's easier.

# Step 5. Calculating the LFP and interpreting the results.

We can now finally use the property checker. Boot up ghci and import megaparsec.hs.
(Note: this parser uses megaparsec which needs to be installed using `cabal install megaparsec`. Other Haskell libraries may need to be installed in the same manner.)
```
ghci megaparsec.hs
```

We can now parse our edited PRG file by typing:

```
prg = parseFromFile sepParse "edited\_NHS\_prg.output"
```

We then calculate the least fixed points of prg:

```
prg >>= parsedLfp
```

This will take several minutes (assuming the file isn't too big, keep it no bigger than 5MB) and will output the results into the file lfp.txt

The output is a single list of tuples, so it's printed on one line. This can be fixed by separating the elements using sed:

```
sed -i 's/),(/),\n(/g' lfp.txt
```

We can then use grep to see whether the elements of the monoid that we would expect to see are contained in the output file (this is easier and faster than using the actual chk function).

```
grep "METH1\_2" lfp.txt
```

# **NB**

I really recommend going through the output from the python code and making sure that the analysis will run correctly i.e that there is a chain of methods referencing one another connecting the monoid elements in the manner that you intend.

Various programming constructs that use generated code break this chain e.g Kotlin coroutines, WorkRequests, Lambda functions, Dependency Injection libraries. Often it is obvious what the output of the python code "should" be, but you have to manually edit it. However, some of these programs use these constructs very heavily which makes this process time-consuming.

As an example, a method example1() may be broken up into two methods, example1$dfe3rf(), which appears in place of example1() in the bytecode, and example1(), which contains the bulk of the original code and is not referenced where we would expect it to be. Moreover, example1$dfe3rf() does not reference example1(). We must manually insert this reference in order for the code to run correctly.

# COMMON PROBLEMS

__I'm getting an error when running parsedLfp__

This is probably because of the coverage of API functions isn't thorough enough. Either add the offending paths to the api\_candidates list in _orderBlocks\_fixed.py_ or, if you think there might only be a handful of problem methods, just use sed to prepend them with an &.

__parsedLfp is taking too long__

parsedLfp isn't that efficient. Try to get the output of _generate\_prg.py_ to be around 5MB by adding paths to the api\_candidates list. Any more than that and it takes a while to run.


__The final results contain no monoid elements__

Make sure that the method source files in step 4. contain the names of the methods you want to replace exactly as they appear in the output from _orderBlocks\_fixed.py_. Otherwise they won't be replaced.
