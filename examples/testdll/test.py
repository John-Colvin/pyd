import platform
import testdll

testdll.foo()

print ("")

print (testdll.bar(12))

print ("")

print ("testdll.baz():")
testdll.baz()
print ("testdll.baz(20):")
testdll.baz(20)
print ("testdll.baz(30, 'cat'):")
testdll.baz(30, 'cat')

print ("")

print ("Testing callback support")
def foo():
    print ("Callback works!")
testdll.dg_test(foo)
print ("Testing delegate wrapping")
dg = testdll.func_test()
dg()

print ("")

print ("Testing class wrapping")
a = testdll.Foo(10)
print ("Class instantiated!")
print ("Testing method wrapping:")
a.foo()
print ("Testing property wrapping:")
print (a.i)
a.i = 50
print (a.i)
print ("Testing operator overloading")
print (a+a)

print ("Testing range iteration wrapping:")
for i in a:
    print (i)

print ("")

print ("Testing exception wrapping")
print ("platform: ", platform.python_version())
if platform.python_version() < "2.6":
    exec("""
try:
    testdll.throws()
except RuntimeError, e:
    print ("Success: Exception caught!")
    print (e)
""")
else:
    exec("""
try:
        testdll.throws()
except RuntimeError as e:
        print ("Success: Exception caught!")
        print (e)
""")

print ("")

S = testdll.S
s = S()
print ("s.s = 'hello'")
s.s = ('hello')
print ("s.s")
print (s.s)
print ("s.write_s()")
s.write_s()

print ("")

print ("Testing custom conversion function")
print (testdll.conv1())
testdll.conv2(20)

print ("")

print ('--------')
print ('SUCCESS')
print ('--------')
