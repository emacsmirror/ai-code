import unittest

from p3841_palindromic_path_queries_in_tree import Solution


class TestSolution(unittest.TestCase):
    def setUp(self):
        self.solution = Solution()

    def test_example_1(self):
        n = 3
        edges = [[0, 1], [1, 2]]
        s = "aac"
        queries = ["query 0 2", "update 1 b", "query 0 2"]
        self.assertEqual([True, False], self.solution.palindromePath(n, edges, s, queries))

    def test_example_2(self):
        n = 4
        edges = [[0, 1], [0, 2], [0, 3]]
        s = "abca"
        queries = ["query 1 2", "update 0 b", "query 2 3", "update 3 a", "query 1 3"]
        self.assertEqual([False, False, True], self.solution.palindromePath(n, edges, s, queries))


if __name__ == "__main__":
    unittest.main()
