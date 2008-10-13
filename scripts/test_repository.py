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

    def testSizes(self):
        self.assert_(len(self.allclasses) >= len(self.topclasses))
        self.assert_(len(self.alldependencies) >= len(self.topdependencies))

    def testSubsets(self):
        self.assert_(self.topclasses.issubset(self.allclasses))
        self.assert_(self.topdependencies.issubset(self.alldependencies))

if __name__ == '__main__':
    unittest.main()
