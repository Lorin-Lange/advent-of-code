////////////////////////////////////////////////////
//              Advent of Code 2022               //
//             Day 24: Blizzard Basin             //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <vector>
#include <queue>
#include <limits>

#define FILE_NAME "test_input.txt"

using namespace std;

enum class color { white, gray, black };

int encode(char c) {
    switch(c) {
        case '.': return 0;
        case '>': return 0 | 1 << 0;
        case '<': return 0 | 1 << 1;
        case '^': return 0 | 1 << 2;
        case 'v': return 0 | 1 << 3;
        case '#': return 0 | 1 << 4;
        default: cout << "Error while encoding." << endl;
                 return -1;
    }
}

unsigned int count_set_bits(unsigned int n) {
    unsigned int count = 0;
    while(n) { count += n & 1; n >>= 1; }
    return count;
}

char decode_show(int i) {
    switch(i) {
        case  0: return '.';
        case  1: return '>';
        case  2: return '<';
        case  4: return '^';
        case  8: return 'v';
        case 16: return '#';
        default: return to_string(count_set_bits(i))[0];
    }
}

void print_grid(vector<vector<int>>& grid) {
    for(auto r : grid) {
        for(auto c : r) cout << decode_show(c);
        cout << "\n";
    }
    cout << "\n";
}

void update_grid(vector<vector<int>>& grid) {
    vector<vector<int>> static_grid = grid;
    const int ROWS = grid.size();
    const int COL = grid[0].size();
    for(int r = 1; r < ROWS - 1; r++) {
        for(int c = 1; c < COL - 1; c++) {

            int res = 0;

            int down_x = r == 1 ? static_grid[ROWS - 2][c] : static_grid[r-1][c];
            if(down_x & 1 << 3) res |= 1 << 3;

            int up_x = r == ROWS - 2 ? static_grid[1][c] : static_grid[r+1][c];
            if(up_x & 1 << 2) res |= 1 << 2;

            int left_x = c == 1 ? static_grid[r][COL - 2] : static_grid[r][c-1];
            if(left_x & 1 << 0) res |= 1 << 0;
            
            int right_x = c == COL - 2 ? static_grid[r][1] : static_grid[r][c+1];
            if(right_x & 1 << 1) res |= 1 << 1;

            grid[r][c] = res;
        }
    }
}


int main() {
    ifstream file;
    file.open(FILE_NAME);
    vector<vector<int>> grid;
    if(file.is_open()) {
        string line;
        while(file) {
            getline(file, line);
            if(!line.empty()) {
                vector<int> row;
                for(auto c : line) {
                    row.push_back(encode(c));
                }
                grid.push_back(row);
            }
        }
        file.close();
    } else {
        cout << "Problem while opening the file." << endl;
        return EXIT_FAILURE;
    }



    print_grid(grid);
    for(int i = 0; i < 10; i++) {
        update_grid(grid);
        print_grid(grid);
    }



    return EXIT_SUCCESS;
}
