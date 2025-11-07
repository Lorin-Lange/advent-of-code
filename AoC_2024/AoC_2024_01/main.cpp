////////////////////////////////////////////////////
//              Advent of Code 2024               //
//           Day 1: Historian Hysteria            //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <map>
#include <cmath>
#include <string>
#include <utility>

using namespace std;

using lists = pair<vector<int>, vector<int>>;

lists parse(const string& path) {
    vector<int> l1, l2;
    ifstream file(path);
    string line;
    while (getline(file, line)) {
        istringstream iss(line);
        int a, b;
        if (iss >> a >> b) {
            l1.push_back(a);
            l2.push_back(b);
        }
    }
    sort(l1.begin(), l1.end());
    sort(l2.begin(), l2.end());
    return pair{l1, l2};
}

int part_1(const lists& l) {
    int sum = 0;
    size_t n = min(l.first.size(), l.second.size());
    for (size_t i = 0; i < n; ++i) {
        sum += abs(l.first[i] - l.second[i]);
    }
    return sum;
}

int part_2(const lists& l) {
    map<int, int> count;
    for (int x : l.second) {
        count[x]++;
    }
    int sum = 0;
    for (int x : l.first) {
        sum += x * count[x];
    }
    return sum;
}

int main() {
    auto pair = parse("input.txt");

    int res = part_1(pair);
    cout << "Part 1: " << res << "\n";

    res = part_2(pair);
    cout << "Part 2: " << res << endl;

    return EXIT_SUCCESS;
}
