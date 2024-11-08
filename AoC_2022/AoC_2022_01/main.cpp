////////////////////////////////////////////////////
//              Advent of Code 2022               //
//            Day 1: Calorie Counting             //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <vector>

#define FILE_NAME "test_input.txt"

using namespace std;

int main() {
    ifstream file;
    file.open(FILE_NAME);
    vector<int> list;
    if(file.is_open()) {
        int sum = 0;
        string line;
        while(file) {
            getline(file, line);
            if(line.empty()) {
                list.push_back(sum);
                sum = 0;
            } else {
                sum += stoi(line);
            }
        }
    }

    sort(list.begin(), list.end(), greater<int>());

    cout << "Part 1: " << list[0] << endl;
    cout << "Part 2: " << list[0] + list[1] + list[2] << endl;

    return EXIT_SUCCESS;
}