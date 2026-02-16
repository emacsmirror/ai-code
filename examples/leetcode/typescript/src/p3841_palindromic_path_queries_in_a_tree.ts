export function palindromePath(n: number, edges: number[][], s: string, queries: string[]): boolean[] {
    // DONE: Implement this function, given requirement, test cases and hints from source: https://leetcode.com/problems/palindromic-path-queries-in-a-tree/description/
    const CODE_A = 97;

    const graph: number[][] = Array.from({ length: n }, () => []);
    for (const [u, v] of edges) {
        graph[u]!.push(v);
        graph[v]!.push(u);
    }

    const parent = new Int32Array(n).fill(-1);
    const depth = new Int32Array(n);
    const tin = new Int32Array(n);
    const tout = new Int32Array(n);
    const order: number[] = [];

    const stack: Array<[number, boolean]> = [[0, false]];
    let timer = 0;

    while (stack.length > 0) {
        const [node, exiting] = stack.pop()!;
        if (exiting) {
            tout[node] = timer - 1;
            continue;
        }

        tin[node] = timer++;
        order.push(node);
        stack.push([node, true]);

        for (const nxt of graph[node]!) {
            if (nxt === parent[node]) continue;
            parent[nxt] = node;
            depth[nxt] = depth[node]! + 1;
            stack.push([nxt, false]);
        }
    }

    const maxJump = Math.ceil(Math.log2(n + 1)) + 1;
    const up: Int32Array[] = Array.from({ length: n }, () => new Int32Array(maxJump).fill(-1));
    for (let i = 0; i < n; i++) up[i]![0] = parent[i]!;
    for (let j = 1; j < maxJump; j++) {
        for (let i = 0; i < n; i++) {
            const mid = up[i]![j - 1]!;
            if (mid !== -1) up[i]![j] = up[mid]![j - 1]!;
        }
    }

    const lca = (a: number, b: number): number => {
        if (depth[a]! < depth[b]!) [a, b] = [b, a];

        let diff = depth[a]! - depth[b]!;
        for (let j = 0; diff > 0; j++, diff >>= 1) {
            if (diff & 1) a = up[a]![j]!;
        }
        if (a === b) return a;

        for (let j = maxJump - 1; j >= 0; j--) {
            if (up[a]![j] !== up[b]![j]) {
                a = up[a]![j]!;
                b = up[b]![j]!;
            }
        }
        return up[a]![0]!;
    };

    const nodeMask = new Int32Array(n);
    for (let i = 0; i < n; i++) {
        nodeMask[i] = 1 << (s.charCodeAt(i) - CODE_A);
    }

    const rootXorBase = new Int32Array(n);
    for (const node of order) {
        if (parent[node] === -1) rootXorBase[node] = nodeMask[node]!;
        else rootXorBase[node] = rootXorBase[parent[node]!]! ^ nodeMask[node]!;
    }

    const bit = new Int32Array(n + 2);
    const bitAdd = (idx: number, delta: number): void => {
        for (let i = idx + 1; i <= n + 1; i += i & -i) bit[i]! ^= delta;
    };
    const bitQuery = (idx: number): number => {
        let res = 0;
        for (let i = idx + 1; i > 0; i -= i & -i) res ^= bit[i]!;
        return res;
    };
    const rangeXor = (l: number, r: number, delta: number): void => {
        bitAdd(l, delta);
        bitAdd(r + 1, delta);
    };

    const getRootXor = (node: number): number => rootXorBase[node]! ^ bitQuery(tin[node]!);
    const getNodeMask = (node: number): number => {
        const p = parent[node]!;
        if (p === -1) return getRootXor(node);
        return getRootXor(node) ^ getRootXor(p);
    };

    const ans: boolean[] = [];
    for (const q of queries) {
        const parts = q.split(' ');
        if (parts[0] === 'update') {
            const u = Number(parts[1]);
            const newMask = 1 << (parts[2]!.charCodeAt(0) - CODE_A);
            const oldMask = getNodeMask(u);
            if (oldMask !== newMask) {
                rangeXor(tin[u]!, tout[u]!, oldMask ^ newMask);
            }
            continue;
        }

        const u = Number(parts[1]);
        const v = Number(parts[2]);
        const c = lca(u, v);
        const pathMask = getRootXor(u) ^ getRootXor(v) ^ getNodeMask(c);
        ans.push((pathMask & (pathMask - 1)) === 0);
    }

    return ans;
}
