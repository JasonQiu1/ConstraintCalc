# project/tests/test.py

import unittest

from project import app

class ProjectTests(unittest.TestCase):
    ############################
    #### setup and teardown ####
    ############################
    
    # executed prior to each test
    def setUp(self):
        app.config['TESTING'] = True
        app.config['DEBUG'] = False
        self.app = app.test_client()
    
        self.assertEquals(app.debug, False)
    
    # executed after each test
    def tearDown(self):
        pass

    def test_main_page(self):
         response = self.app.get('/', follow_redirects=True)
         self.assertIn(b'Welcome to Constraint Calculator!', response.data)

if __name__ == "__main__":
    unittest.main()
