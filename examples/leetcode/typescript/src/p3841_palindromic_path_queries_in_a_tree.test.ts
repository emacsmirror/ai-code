import { describe, test, expect } from 'vitest';
import { palindromePath } from './p3841_palindromic_path_queries_in_a_tree';

describe('palindromePath', () => {
  test('Example 1 from statement', () => {
    const n = 3;
    const edges = [[0, 1], [1, 2]];
    const s = 'aac';
    const queries = ['query 0 2', 'update 1 b', 'query 0 2'];

    expect(palindromePath(n, edges, s, queries)).toEqual([true, false]);
  });

  test('Example 2 from statement', () => {
    const n = 4;
    const edges = [[0, 1], [0, 2], [0, 3]];
    const s = 'abca';
    const queries = ['query 1 2', 'update 0 b', 'query 2 3', 'update 3 a', 'query 1 3'];

    expect(palindromePath(n, edges, s, queries)).toEqual([false, false, true]);
  });

  test('Single-node path is always palindromic', () => {
    const n = 2;
    const edges = [[0, 1]];
    const s = 'ab';
    const queries = ['query 1 1', 'update 1 c', 'query 0 1'];

    expect(palindromePath(n, edges, s, queries)).toEqual([true, false]);
  });
});
