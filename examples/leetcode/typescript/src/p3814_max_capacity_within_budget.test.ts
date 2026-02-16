import { describe, test, expect } from 'vitest';
import { maxCapacity } from './p3814_max_capacity_within_budget';

describe('maxCapacity', () => {
  test('Example 1: costs = [4,8,5,3], capacity = [1,5,2,7], budget = 8', () => {
    const result = maxCapacity([4, 8, 5, 3], [1, 5, 2, 7], 8);
    expect(result).toBe(8);
  });

  test('Example 2: costs = [3,5,7,4], capacity = [2,4,3,6], budget = 7', () => {
    const result = maxCapacity([3, 5, 7, 4], [2, 4, 3, 6], 7);
    expect(result).toBe(6);
  });

  test('Example 3: costs = [2,2,2], capacity = [3,5,4], budget = 5', () => {
    const result = maxCapacity([2, 2, 2], [3, 5, 4], 5);
    expect(result).toBe(9);
  });

  test('No machine can be selected when all costs are not strictly less than budget', () => {
    const result = maxCapacity([5, 6], [10, 20], 5);
    expect(result).toBe(0);
  });
});
