#!/bin/python

from pywbem import *
import unittest

cli = WBEMConnection('http://localhost')

class EnumerateClassNames(unittest.TestCase):

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

    def test(self):
        [self.assertEqual(cl, cli.GetClass(cl.classname)) 
         for cl in self.allclasses]

class CreateClass(unittest.TestCase):

    name = 'Griffin_TestCreateClass'

    def test(self):

        cl = CIMClass(
            self.name, 
            properties = {'InstanceID':
                              CIMProperty('InstanceID', None, 'string')})

        cli.CreateClass(cl)

        self.assertEqual(cli.GetClass(self.name), cl)

    def tearDown(self):
        cli.DeleteClass(self.name)

class DeleteClass(unittest.TestCase):

    name = 'Griffin_TestDeleteClass'

    def setUp(self):

        cl = CIMClass(
            self.name, 
            properties = {'InstanceID':
                              CIMProperty('InstanceID', None, 'string')})

        cli.CreateClass(cl)

    def test(self):
        cli.DeleteClass(self.name)
        self.assertRaises(CIMError, cli.GetClass, self.name)

class ModifyClass(unittest.TestCase):

    name = 'Griffin_TestModifyClass'

    def setUp(self):

        self.cl = CIMClass(
            self.name, 
            properties = {'InstanceID':
                              CIMProperty('InstanceID', None, 'string')})

        cli.CreateClass(self.cl)
        
    def test(self):

        newcl = self.cl.copy()
        newcl.properties['Foo'] = CIMProperty('Foo', None, 'boolean')

        cli.ModifyClass(newcl)

        self.assertEqual(cli.GetClass(self.name), newcl)

    def tearDown(self):
        cli.DeleteClass(self.name)

class EnumerateQualifiers(unittest.TestCase):
    
    def test(self):
        quals = cli.EnumerateQualifiers()
        self.assert_(len(quals) > 0)

class GetQualifier(unittest.TestCase):

    def test(self):
        quals = cli.EnumerateQualifiers()
        [self.assertEqual(q, cli.GetQualifier(q.name)) for q in quals]

class DeleteQualifier(unittest.TestCase):

    name = 'Griffin_TestDeleteQualifier'

    def setUp(self):
        cli.SetQualifier(CIMQualifierDeclaration(self.name, 'boolean'))

    def test(self):
        cli.DeleteQualifier(self.name)
        self.assertRaises(CIMError, cli.GetQualifier, self.name)

class SetQualifier(unittest.TestCase):

    name = 'Griffin_TestSetQualifier'

    def test(self):
        qd = CIMQualifierDeclaration(self.name, 'boolean')
        cli.SetQualifier(qd)
        self.assertEqual(cli.GetQualifier(self.name), qd)

    def tearDown(self):
        cli.DeleteQualifier(self.name)

if __name__ == '__main__':
    unittest.main()
