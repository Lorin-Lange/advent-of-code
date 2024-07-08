////////////////////////////////////////////////////
//              Advent of Code 2022               //
//        Day 12: Hill Climbing Algorithm         //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <vector>
#include <queue>
#include <limits>

#define FILE_NAME "input.txt"

using namespace std;

enum class color { white, gray, black };

struct S {
    char c;
    color col;
    int distance;
    pair<int, int> pos;
};

bool comp_chars(char my_pos, char target) {
    if(target == 'E') return (int)'z' - 48 - 1 <= (int)my_pos - 48;
    if(my_pos == 'S') return (int)target - 48 - 1 <= (int)'a' - 48;
    return (int)target - 48 - 1 <= (int)my_pos - 48;
}

int bfs(const pair<int, int> s, const vector<string> grid) {
    const int MAX_X = grid[0].length();
    const int MAX_Y = grid.size();

    S new_grid[MAX_Y][MAX_X];
    for(int i = 0; i < MAX_Y; i++) {
        for(int j = 0; j < MAX_X; j++) {
            S s;
            s.c = grid[i][j];
            s.col = color::white;
            s.distance = numeric_limits<int>::max();
            s.pos = pair(j, i);
            new_grid[i][j] = s;
        }
    }

    S start;
    start.c = grid[s.second][s.first];
    start.col = color::gray;
    start.distance = 0;
    start.pos = s;

    queue<S> queue;
    queue.push(start);


    while(!queue.empty()) {
        auto p = queue.front();
        queue.pop();

        if(p.c == 'E') return p.distance;

        if(p.pos.first + 1 < MAX_X &&
           new_grid[p.pos.second][p.pos.first + 1].col == color::white &&
           comp_chars(p.c, new_grid[p.pos.second][p.pos.first + 1].c)) {
            S local = new_grid[p.pos.second][p.pos.first + 1];
            local.col = color::gray;
            local.distance = p.distance + 1;
            new_grid[p.pos.second][p.pos.first + 1] = local;
            queue.push(local);
        }

        if(p.pos.first - 1 >= 0 &&
           new_grid[p.pos.second][p.pos.first - 1].col == color::white &&
           comp_chars(p.c, new_grid[p.pos.second][p.pos.first - 1].c)) {
            S local = new_grid[p.pos.second][p.pos.first - 1];
            local.col = color::gray;
            local.distance = p.distance + 1;
            new_grid[p.pos.second][p.pos.first - 1] = local;
            queue.push(local);
        }

        if(p.pos.second + 1 < MAX_Y &&
           new_grid[p.pos.second + 1][p.pos.first].col == color::white &&
           comp_chars(p.c, new_grid[p.pos.second + 1][p.pos.first].c)) {
            S local = new_grid[p.pos.second + 1][p.pos.first];
            local.col = color::gray;
            local.distance = p.distance + 1;
            new_grid[p.pos.second + 1][p.pos.first] = local;
            queue.push(local);
        }

        if(p.pos.second - 1 >= 0 &&
           new_grid[p.pos.second - 1][p.pos.first].col == color::white &&
           comp_chars(p.c, new_grid[p.pos.second - 1][p.pos.first].c)) {
            S local = new_grid[p.pos.second - 1][p.pos.first];
            local.col = color::gray;
            local.distance = p.distance + 1;
            new_grid[p.pos.second - 1][p.pos.first] = local;
            queue.push(local);
        }

        new_grid[p.pos.second][p.pos.first].col = color::black; 
    }

    return numeric_limits<int>::max();
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
    }
    file.close();

    // Part 1:
    pair<int, int> start;
    for(int x = 0; x < grid[0].length(); x++) {
        for(int y = 0; y < grid.size(); y++) {
            if(grid[y][x] == 'S') {
                start.first = x;
                start.second = y;
                break;
            }
        }
    }

    cout << "Part 1: " << bfs(start, grid) << endl;

    // Part 2:
    vector<pair<int, int>> start_positions;
    start_positions.push_back(start);
    for(int x = 0; x < grid[0].length(); x++) {
        for(int y = 0; y < grid.size(); y++) {
            if(grid[y][x] == 'a') {
                pair<int, int> p(x,y);
                start_positions.push_back(p);
            }
        }
    }

    vector<int> paths;
    for(auto p : start_positions) {
        paths.push_back(bfs(p, grid));
    }
    auto min = min_element(paths.begin(), paths.end()); 
    cout << "Part 2: " << *min << endl;

    return EXIT_SUCCESS;
}