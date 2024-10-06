////////////////////////////////////////////////////
//              Advent of Code 2022               //
//            Day 25: Full of Hot Air             //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <vector>
#include <cmath>

#define FILE_NAME "input.txt"

using namespace std;

long SNAFU_to_decimal(string snafu) {
    reverse(snafu.begin(), snafu.end());
    long res_sum = 0;
    for(int i = 0; i < snafu.size(); i++) {
        switch(snafu[i]) {
            case '=': res_sum += -2 * pow(5, i); break;
            case '-': res_sum += -1 * pow(5, i); break;
            case '0': res_sum +=  0 * pow(5, i); break;
            case '1': res_sum +=  1 * pow(5, i); break;
            case '2': res_sum +=  2 * pow(5, i); break;
        }
    }
    return res_sum;
}

char int_to_SNAFU_char(int i) {
    switch(i) {
        case 0: return '=';
        case 1: return '-';
        case 2: return '0';
        case 3: return '1';
        case 4: return '2';
    }
    return ' ';
} 

string decimal_to_SNAFU(long decimal) {
    string res = "";
    while(decimal != 0) {        
        res += int_to_SNAFU_char((decimal + 2) % 5);
        decimal = (decimal + 2) / 5;
    }
    reverse(res.begin(), res.end());
    return res;
}

int main() {
    ifstream file;
    file.open(FILE_NAME);
    vector<string> list;
    if(file.is_open()) {
        string line;
        while(file) {
            getline(file, line);
            if(!line.empty()) list.push_back(line);
        }
    } else {
        cout << "Problem while opening the file." << endl;
        return EXIT_FAILURE;
    }

    long sum = 0;
    for(auto s : list) sum += SNAFU_to_decimal(s);
    cout << "Result: " << decimal_to_SNAFU(sum) << endl;
    
    return EXIT_SUCCESS;
}
