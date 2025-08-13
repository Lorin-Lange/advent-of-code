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

pair<int, int> find_start(vector<string> &g) {
    for(int r = 0; r < g.size(); r++) {
        for(int c = 0; c < g[r].size(); c++) {
            if(g[r][c] == '^') {
                g[r][c] == '.';
                return make_pair(r, c);
            }
        }
    }
    return make_pair(-1, -1);
}

int part_one(pair<int, int> pos, vector<string> &grid) {
    vector<pair<int, int>> dirs = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
    int dir = 0;
    int height = grid.size();
    int width = grid[0].size();
    set<pair<int, int>> visited;
    pair<int, int> next_dir;
    while(true) {
        next_dir = dirs[dir];
        int r = pos.first + next_dir.first;
        int c = pos.second + next_dir.second;
        if(r < 0 || r >= height || c < 0 || c >= width) break;
        if (grid[r][c] == '.') {
            pos = {r, c};
            visited.insert(pos);
        } else {
            dir = (dir + 1) % 4;
        }
    }
    return visited.size();
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
    auto start_pos = find_start(grid);

    cout << "Part 1: ";
    cout << part_one(start_pos, grid) << "\n";

    cout << "Part 2: ";
    cout << "\n";





    return EXIT_SUCCESS;
}