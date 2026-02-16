import unittest

from p3828_final_element_after_subarray_deletions import Solution


class TestSolution(unittest.TestCase):
    def setUp(self):
        self.solution = Solution()

    def test_example_1(self):
        self.assertEqual(self.solution.finalElement([1, 5, 2]), 2)

    def test_example_2(self):
        self.assertEqual(self.solution.finalElement([3, 7]), 7)

    def test_single_element(self):
        self.assertEqual(self.solution.finalElement([9]), 9)

    def test_choose_better_endpoint(self):
        self.assertEqual(self.solution.finalElement([8, 1, 6, 2]), 8)


if __name__ == "__main__":
    unittest.main()
