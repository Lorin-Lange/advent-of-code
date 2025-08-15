////////////////////////////////////////////////////
//              Advent of Code 2024               //
//             Day 6: Guard Gallivant             //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <vector>
#include <utility>
#include <set>

#define FILE_NAME "input.txt"

using namespace std;

vector<pair<int, int>> dirs = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};

pair<int, int> find_start(vector<string> &g) {
    for(int r = 0; r < g.size(); r++) {
        for(int c = 0; c < g[r].size(); c++) {
            if(g[r][c] == '^') {
                g[r][c] = '.';
                return make_pair(r, c);
            }
        }
    }
    return make_pair(-1, -1);
}

int part_one(pair<int, int> pos, vector<string> &g) {
    int height = g.size();
    int width = g[0].size();
    set<pair<int, int>> visited;
    int dir = 0;
    while(true) {
        visited.insert(pos);
        int r = pos.first + dirs[dir].first;
        int c = pos.second + dirs[dir].second;
        if(r < 0 || r >= height || c < 0 || c >= width) break;
        if(g[r][c] == '.') pos = {r, c};
        else dir = (dir + 1) % 4;
    }
    return visited.size();
}

int part_two(pair<int, int> pos, vector<string> &g) {
    return -1;
}

int main() {
    ifstream file;
    file.open(FILE_NAME);
    vector<string> grid;
    if(file.is_open()) {
        string line;
        while(file) {
            getline(file, line);
            if(!line.empty()) {
                grid.push_back(line);
            }
        }
        file.close();
    } else {
        cout << "Problem while opening the file." << endl;
        return EXIT_FAILURE;
    }

    pair<int, int> start_pos = find_start(grid);

    cout << "Part 1: " << part_one(start_pos, grid) << "\n";
    cout << "Part 2: " << part_two(start_pos, grid) << endl;

    return EXIT_SUCCESS;
}
