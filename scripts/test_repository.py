#!/bin/python

from pywbem import *
import unittest

cli = WBEMConnection('http://localhost')

class EnumerateClassNamesTest(unittest.TestCase):

    def setUp(self):

        self.allclasses = set(cli.EnumerateClassNames(DeepInheritance = True))
        self.topclasses = set(cli.EnumerateClassNames(DeepInheritance = False))

        cl = 'CIM_Dependency'

        self.alldependencies = set(cli.EnumerateClassNames(
                ClassName = cl, DeepInheritance = True))

        self.topdependencies = set(cli.EnumerateClassNames(
                ClassName = cl, DeepInheritance = False))

    def testNotEmpty(self):
        self.assert_(len(self.allclasses) > 0)
        self.assert_(len(self.topclasses) > 0)
        self.assert_(len(self.alldependencies) > 0)
        self.assert_(len(self.topdependencies) > 0)

    def testSizes(self):
        self.assert_(len(self.allclasses) >= len(self.topclasses))
        self.assert_(len(self.alldependencies) >= len(self.topdependencies))

    def testSubsets(self):
        self.assert_(self.topclasses.issubset(self.allclasses))
        self.assert_(self.topdependencies.issubset(self.alldependencies))

class EnumerateClasses(unittest.TestCase):

    def setUp(self):

        self.allclasses = set(cli.EnumerateClasses(DeepInheritance = True))
        self.topclasses = set(cli.EnumerateClasses(DeepInheritance = False))

        cl = 'CIM_Dependency'

        self.alldependencies = set(cli.EnumerateClasses(
                ClassName = cl, DeepInheritance = True))

        self.topdependencies = set(cli.EnumerateClasses(
                ClassName = cl, DeepInheritance = False))

    def testNotEmpty(self):
        self.assert_(len(self.allclasses) > 0)
        self.assert_(len(self.topclasses) > 0)
        self.assert_(len(self.alldependencies) > 0)
        self.assert_(len(self.topdependencies) > 0)

    def testSizes(self):
        self.assert_(len(self.allclasses) >= len(self.topclasses))
        self.assert_(len(self.alldependencies) >= len(self.topdependencies))

    def testSubsets(self):
        self.assert_(self.topclasses.issubset(self.allclasses))
        self.assert_(self.topdependencies.issubset(self.alldependencies))

class GetClass(unittest.TestCase):

    def setUp(self):
        self.allclasses = set(cli.EnumerateClasses(DeepInheritance = True))

    def testGetClass(self):
        [self.assertEqual(cl, cli.GetClass(cl.classname)) 
         for cl in self.allclasses]

if __name__ == '__main__':
    unittest.main()
