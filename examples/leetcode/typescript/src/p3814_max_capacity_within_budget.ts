
// You are given two integer arrays costs and capacity, both of length n, where costs[i] represents the purchase cost of the ith machine and capacity[i] represents its performance capacity.

// You are also given an integer budget.

// You may select at most two distinct machines such that the total cost of the selected machines is strictly less than budget.

// Return the maximum achievable total capacity of the selected machines.

 

// Example 1:

// Input: costs = [4,8,5,3], capacity = [1,5,2,7], budget = 8

// Output: 8

// Explanation:

//     Choose two machines with costs[0] = 4 and costs[3] = 3.
//     The total cost is 4 + 3 = 7, which is strictly less than budget = 8.
//     The maximum total capacity is capacity[0] + capacity[3] = 1 + 7 = 8.

// Example 2:

// Input: costs = [3,5,7,4], capacity = [2,4,3,6], budget = 7

// Output: 6

// Explanation:

//     Choose one machine with costs[3] = 4.
//     The total cost is 4, which is strictly less than budget = 7.
//     The maximum total capacity is capacity[3] = 6.

// Example 3:

// Input: costs = [2,2,2], capacity = [3,5,4], budget = 5

// Output: 9

// Explanation:

//     Choose two machines with costs[1] = 2 and costs[2] = 2.
//     The total cost is 2 + 2 = 4, which is strictly less than budget = 5.
//     The maximum total capacity is capacity[1] + capacity[2] = 5 + 4 = 9.

 

// Constraints:

//     1 <= n == costs.length == capacity.length <= 10^5
//     1 <= costs[i], capacity[i] <= 10^5
//     1 <= budget <= 2 * 10^5

 
// Hint 1
// Sort machines by increasing costs, keeping capacity aligned.
// Hint 2
// Build a prefix array where prefMax[i] stores the maximum capacity among machines with index <= i.
// Hint 3
// For selecting one machine, take the maximum capacity among all machines with cost < budget.
// Hint 4
// For selecting two machines, fix machine i and binary search the largest index j < i with costs[j] < budget - costs[i].
// Hint 5
// Update the answer with capacity[i] + prefMax[j] whenever such j exists.


export function maxCapacity(costs: number[], capacity: number[], budget: number): number {
    const n = costs.length;
    const machines: Array<{ cost: number; cap: number }> = [];

    for (let i = 0; i < n; i++) {
        machines.push({ cost: costs[i]!, cap: capacity[i]! });
    }

    machines.sort((a, b) => a.cost - b.cost);

    const sortedCosts = machines.map((m) => m.cost);
    const prefixMaxCap: number[] = Array(n).fill(0);
    for (let i = 0; i < n; i++) {
        const current = machines[i]!.cap;
        prefixMaxCap[i] = i === 0 ? current : Math.max(prefixMaxCap[i - 1]!, current);
    }

    let answer = 0;

    // Choose one machine.
    for (let i = 0; i < n; i++) {
        if (machines[i]!.cost < budget) {
            answer = Math.max(answer, machines[i]!.cap);
        }
    }

    // Choose two distinct machines.
    for (let i = 1; i < n; i++) {
        const remain = budget - machines[i]!.cost;
        if (remain <= 0) continue;

        const j = upperBound(sortedCosts, remain - 1, i - 1);
        if (j >= 0) {
            answer = Math.max(answer, machines[i]!.cap + prefixMaxCap[j]!);
        }
    }

    return answer;
}

function upperBound(arr: number[], target: number, hi: number): number {
    let left = 0;
    let right = hi;
    let ans = -1;

    while (left <= right) {
        const mid = left + ((right - left) >> 1);
        if (arr[mid]! <= target) {
            ans = mid;
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }

    return ans;
}
